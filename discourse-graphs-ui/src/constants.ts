/** Graph physics simulation configuration */
export const PHYSICS = {
  /** Repulsion force strength between nodes */
  CHARGE_STRENGTH: 50,
  /** Maximum distance for charge force effect */
  CHARGE_DISTANCE_MAX: 300,
  /** Default distance between linked nodes */
  LINK_DISTANCE: 150,
  /** Velocity decay factor (0-1, higher = faster stop) */
  VELOCITY_DECAY: 0.4,
  /** Number of ticks for simulation cooldown */
  COOLDOWN_TICKS: 100,
  /** Number of ticks for simulation warmup */
  WARMUP_TICKS: 50,
  /** Center force strength */
  CENTER_STRENGTH: 0.05,
  /** Radial force strength */
  RADIAL_STRENGTH: 0.1,
  /** Radial force radius */
  RADIAL_RADIUS: 50,
} as const;

/** Node rendering configuration */
export const NODE = {
  /** Base size for nodes */
  RELATIVE_SIZE: 8,
  /** Label visibility threshold (global scale) */
  LABEL_VISIBILITY_THRESHOLD: 0.5,
  /** Minimum line width for links */
  MIN_LINK_WIDTH: 0.6,
  /** Maximum line width for links */
  MAX_LINK_WIDTH: 3,
} as const;

/** Animation timing configuration (milliseconds) */
export const TIMING = {
  /** Duration for centering on a node */
  CENTER_DURATION: 1000,
  /** Duration for zoom animation */
  ZOOM_DURATION: 300,
  /** Delay before auto-fitting view */
  AUTO_FIT_DELAY: 300,
  /** Duration for auto-fit animation */
  AUTO_FIT_DURATION: 1000,
  /** Toast notification duration */
  TOAST_DURATION: 3000,
  /** Window resize debounce delay */
  RESIZE_DEBOUNCE: 100,
  /** Double-click detection window */
  DOUBLE_CLICK_WINDOW: 300,
  /** Resize transition duration */
  RESIZE_TRANSITION: 300,
} as const;

/** UI layout configuration */
export const LAYOUT = {
  /** Sidebar width in pixels */
  SIDEBAR_WIDTH: 280,
  /** Padding for zoom-to-fit */
  FIT_PADDING: 80,
  /** Default zoom level when centering on node */
  DEFAULT_ZOOM: 2.5,
} as const;

/** Theme colors */
export const THEME = {
  DARK: {
    BACKGROUND: '#0a0e17',
  },
  LIGHT: {
    BACKGROUND: '#ffffff',
  },
} as const;
