// Use strict mode if available.
"use strict";

/*
 * Enumerations.
 */

// Orientation enumeration.
Enum.define('Orientation', {
    HORIZONTAL: 'horizontal',
    VERTICAL: 'vertical'
});

// Shadow type enumeration.
Enum.define('ShadowType', {
    NONE: 'none',
    IN: 'in',
    OUT: 'out',
    ETCHED_IN: 'etched-in',
    ETCHED_OUT: 'etched-out'
});

// Justify enumeration.
Enum.define('Justify', { // TODO: Use align?
    LEFT: 'left', // TODO: start?
    RIGHT: 'right', // TODO: end?
    CENTER: 'center',
    FILL: 'fill'
});

// Scroll bar policies.
Enum.define('Policy', {
    ALWAYS: 'always',
    AUTOMATIC: 'automatic',
    NEVER: 'never'
});

Enum.define('EventModifierMask', {
    // Intentional gap.
    
    PRIMARY_BUTTON:    1 << 1,
    MIDDLE_BUTTON:     1 << 2,
    SECONDARY_BUTTON3: 1 << 3,
    
    // Intentional gap.
    
    SHIFT:   1 << 6,
    CONTROL: 1 << 7,
    ALT:     1 << 8,
    SUPER:   1 << 9,
    
    // Combinations.
    BUTTONS: (1 << 1 | 1 << 2 | 1 << 3),
    NONE: 0,
    ALL: ~0
});

Enum.define('EventType', {
    MOTION: 'motion',
    SCROLL: 'scroll',
    
    ENTER: 'enter',
    LEAVE: 'leave',
    
    KEY_PRESS: 'key-press',
    KEY_RELEASE: 'key-release',
    
    BUTTON_PRESS: 'button-press',
    BUTTON_RELEASE: 'button-release',
    
    FOCUS: 'focus',
    BLUR: 'blur'
});

Enum.define('EventMask', {
    // Button, key, motion, scroll, enter, leave, touch
    
    // Even shifts.
    MOTION: 1 << 0,
    SCROLL: 1 << 2,
    
    ENTER: 1 << 4, // Does not bubble.
    LEAVE: 1 << 6, // Does not bubble.
    
    KEY_PRESS:   1 <<  8,
    KEY_RELEASE: 1 << 10,
    
    BUTTON_PRESS:   1 << 12,
    BUTTON_RELEASE: 1 << 14,
    
    FOCUS: 1 << 16, // Does not bubble.
    BLUR:  1 << 18, // Does not bubble.
    
    //OVER: 1 << 20,
    //OUT:  1 << 22,
    
    //FOCUS_IN:  1 << 24,
    //FOCUS_OUT: 1 << 26,
    
    
    
    // Uneven shifts.
    CAPTURE_MOTION: 1 << 1,
    CAPTURE_SCROLL: 1 << 3,
    
    CAPTURE_ENTER: 1 << 5,
    CAPTURE_LEAVE: 1 << 7,
    
    CAPTURE_KEY_PRESS:   1 << 9,
    CAPTURE_KEY_RELEASE: 1 << 11,
    
    CAPTURE_BUTTON_PRESS:   1 << 13,
    CAPTURE_BUTTON_RELEASE: 1 << 15,
    
    CAPTURE_FOCUS: 1 << 17,
    CAPTURE_BLUR:  1 << 19,
    
    // Combinations.
    NONE: 0,
    ALL: ~0,
    CAPTURE_ALL: 0xAAAAAAAA // 0b10101010101010101010101010101010
});

/**
 * Button enumeration.
 */
Enum.define('Button', {
    /**
     * Primary button of a pointer device.
     * This is typically the left mouse button in a right-handed mouse configuration.
     */
    PRIMARY: 1,
    
    /**
     * Middle button of a pointer device.
     */
    MIDDLE: 2,
    
    /**
     * Secondary button of a pointer device.
     * This is typically the right mouse button in a right-handed mouse configuration.
     */
    SECONDARY: 3
});

/**
 * Key enumeration.
 */
Enum.define('Key', {
    BACKSPACE: 8,
    TAB: 9, // Printable.
    RETURN: 13,
    ENTER: 13,
    SHIFT: 18,
    CONTROL: 17,
    ALT: 18,
    PAUSE: 19,
    BREAK: 19, // Same key as pause.
    CAPS_LOCK: 20,
    ESCAPE: 27,
    SPACE: 32, // Printable.
    PAGE_UP: 33,
    PAGE_DOWN: 34,
    END: 35,
    HOME: 36,
    LEFT: 37,
    UP: 38,
    RIGHT: 39,
    DOWN: 40,
    INSERT: 45,
    DEL: 46,
    
    // Printable.
    A: 65,
    B: 66,
    C: 67,
    D: 68,
    E: 69,
    F: 70,
    G: 71,
    H: 72,
    I: 73,
    J: 74,
    K: 75,
    L: 76,
    M: 77,
    N: 78,
    O: 79,
    P: 80,
    Q: 81,
    R: 82,
    S: 83,
    T: 84,
    U: 85,
    V: 86,
    W: 87,
    X: 88,
    Y: 89,
    Z: 90,
    
    //Added (left, right)
    //91: 'super',
    //92: 'super',
    //93: 'select', //context menu
    
    // Printable.
    KP_0: 96,
    KP_1: 97,
    KP_2: 98,
    KP_3: 99,
    KP_4: 100,
    KP_5: 101,
    KP_6: 102,
    KP_7: 103,
    KP_8: 104,
    KP_9: 105,
    
    // Printable.
    KP_MULTIPLY: 106,
    KP_ADD: 107,
    KP_SEPARATOR: 108, // Valid? Printable?
    KP_SUBTRACT: 109,
    KP_DECIMAL: 110,
    KP_DIVIDE: 111,
    
    F1: 112,
    F2: 113,
    F3: 114,
    F4: 115,
    F5: 116,
    F6: 117,
    F7: 118,
    F8: 119,
    F9: 120,
    F10: 121,
    F11: 122,
    F12: 123,
    
    NUM_LOCK: 144,
    SCROLL_LOCK: 145,
    
    // Printable.
    SEMICOLON: 186,
    EQUAL: 187,
    COMMA: 188,
    MINUS: 189,
    PERIOD: 190,
    SLASH: 191
    
    
    //Added:
    //192: '`',
    //219: '[',
    //220: '\\',
    //221: ']',
    //222: '\'',

    // Remove?
    //224: 'meta'
    
});
