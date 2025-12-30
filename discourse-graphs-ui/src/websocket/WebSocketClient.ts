import { WebSocketMessage, GraphData } from '../types';

/** WebSocket reconnection configuration */
const RECONNECT_CONFIG = {
  /** Initial delay before first reconnection attempt (ms) */
  INITIAL_DELAY: 1000,
  /** Maximum delay between reconnection attempts (ms) */
  MAX_DELAY: 30000,
  /** Multiplier for exponential backoff */
  BACKOFF_MULTIPLIER: 2,
  /** Maximum number of reconnection attempts (0 = unlimited) */
  MAX_RETRIES: 10,
} as const;

type MessageHandler<T = unknown> = (data: T) => void;

interface HandlerMap {
  connected: MessageHandler<null>;
  disconnected: MessageHandler<null>;
  error: MessageHandler<{ message: string; code?: number }>;
  graphdata: MessageHandler<GraphData>;
  focus: MessageHandler<{ id: string }>;
  command: MessageHandler<{ commandName: string; id?: string }>;
  theme: MessageHandler<{ colors?: Record<string, string> }>;
}

type EventType = keyof HandlerMap;

export class WebSocketClient {
  private ws: WebSocket | null = null;
  private messageHandlers: Map<string, MessageHandler<unknown>> = new Map();
  private reconnectAttempts = 0;
  private reconnectTimeout: number | null = null;
  private isManuallyDisconnected = false;

  constructor(private url: string) {}

  connect(): void {
    this.isManuallyDisconnected = false;
    this.reconnectAttempts = 0;
    this.doConnect();
  }

  private doConnect(): void {
    // Clear any pending reconnect
    if (this.reconnectTimeout !== null) {
      clearTimeout(this.reconnectTimeout);
      this.reconnectTimeout = null;
    }

    try {
      this.ws = new WebSocket(this.url);
    } catch (error) {
      console.error('❌ Failed to create WebSocket:', error);
      this.scheduleReconnect();
      return;
    }

    this.ws.onopen = () => {
      console.log('✅ WebSocket connected');
      this.reconnectAttempts = 0; // Reset on successful connection
      this.emit('connected', null);
      this.send({ type: 'requestGraphData' });
    };

    this.ws.onmessage = (event) => {
      try {
        const message = JSON.parse(event.data) as WebSocketMessage;
        if (!message.type) {
          console.warn('Received message without type:', message);
          return;
        }
        this.emit(message.type, message.data);
      } catch (error) {
        console.error('Error parsing WebSocket message:', error);
        this.emit('error', {
          message: 'Failed to parse server message',
          code: undefined
        });
      }
    };

    this.ws.onerror = (event) => {
      console.error('❌ WebSocket error:', event);
      this.emit('error', {
        message: 'WebSocket connection error',
        code: undefined
      });
    };

    this.ws.onclose = (event) => {
      console.log(`🔴 WebSocket disconnected (code: ${event.code})`);
      this.emit('disconnected', null);

      if (!this.isManuallyDisconnected) {
        this.scheduleReconnect();
      }
    };
  }

  private scheduleReconnect(): void {
    if (RECONNECT_CONFIG.MAX_RETRIES > 0 &&
        this.reconnectAttempts >= RECONNECT_CONFIG.MAX_RETRIES) {
      console.error(`❌ Max reconnection attempts (${RECONNECT_CONFIG.MAX_RETRIES}) reached`);
      this.emit('error', {
        message: `Failed to reconnect after ${RECONNECT_CONFIG.MAX_RETRIES} attempts`,
        code: undefined
      });
      return;
    }

    // Calculate delay with exponential backoff
    const delay = Math.min(
      RECONNECT_CONFIG.INITIAL_DELAY * Math.pow(RECONNECT_CONFIG.BACKOFF_MULTIPLIER, this.reconnectAttempts),
      RECONNECT_CONFIG.MAX_DELAY
    );

    this.reconnectAttempts++;
    console.log(`🔄 Reconnecting in ${delay}ms (attempt ${this.reconnectAttempts}/${RECONNECT_CONFIG.MAX_RETRIES || '∞'})...`);

    this.reconnectTimeout = window.setTimeout(() => {
      this.doConnect();
    }, delay);
  }

  on<K extends EventType>(event: K, handler: HandlerMap[K]): void {
    this.messageHandlers.set(event, handler as MessageHandler<unknown>);
  }

  private emit<K extends EventType>(event: K, data: Parameters<HandlerMap[K]>[0]): void {
    const handler = this.messageHandlers.get(event);
    if (handler) {
      handler(data);
    }
  }

  send(message: Record<string, unknown>): void {
    if (this.ws && this.ws.readyState === WebSocket.OPEN) {
      this.ws.send(JSON.stringify(message));
    } else {
      console.warn('Cannot send message: WebSocket is not connected');
    }
  }

  disconnect(): void {
    this.isManuallyDisconnected = true;
    if (this.reconnectTimeout !== null) {
      clearTimeout(this.reconnectTimeout);
      this.reconnectTimeout = null;
    }
    if (this.ws) {
      this.ws.close();
      this.ws = null;
    }
  }

  isConnected(): boolean {
    return this.ws !== null && this.ws.readyState === WebSocket.OPEN;
  }
}
