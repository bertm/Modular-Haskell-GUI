// Use strict mode if available.
"use strict";

// Save browser event reference.
//var BrowserEvent = Event; // TODO: Not in IE.

/**
 * Event base class.
 */
Class.define('Event', {
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers)
    {
        this.date      = new Date();
        this.source    = source;
        this.modifiers = modifiers;
        
        // We are not constructing the instance here, so
        // a signal dispatcher is not created.
    },
    
    /*
     * Disable signal proxy methods.
     */
    
    connect: function() { },
    connectFirst: function() { },
    connectLast: function() { },
    disconnect: function() { },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The type of the event.
         *
         * @type EventType
         */
        type: {
            read: true,
            defaultValue: null
        },
        /**
         * The timestamp of the event in UTC milliseconds since epoch.
         *
         * @type int
         */
        timestamp: {
            read: function()
            {
                return +this.date - this.date.getTimezoneOffset() * 60000;
            }
        },
        /**
         * The source #Widget of the event.
         *
         * @type Widget
         */
        source: {
            read: true,
            defaultValue: null
        },
        /**
         * The modifier mask of the event.
         *
         * @type int
         * @see EventModifierMask
         */
        modifiers: {
            read: true,
            defaultValue: EventModifierMask.NONE
        },
    },
    
    /*
     * Actions.
     */
    
    actions: {
        /**
         * Checks whether a modifier is enabled. Does the same as `event.getModifiers() & modifier`.
         *
         * @param EventModifierMask modifier Modifier to check agains.
         * @return bool Whether the modifier is enabled.
         */
        hasModifier: function(modifier)
        {
            return (this.modifiers & modifier);
        }
    }
});

/**
 * Pointer event.
 */
Class.define('PointerEvent', {
    extend: 'Event',
    
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers, x, y)
    {
        this.x = x;
        this.y = y;
        
        PointerEvent.base.construct.call(this, source, modifiers);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The x position of the pointer.
         *
         * @type int
         */
        x: { // TODO: Both, or one of them?
            read: true,
            defaultValue: 0
        },
        /**
         * The y position of the pointer.
         *
         * @type int
         */
        y: {
            read: true,
            defaultValue: 0
        },
        /**
         * The position of the pointer.
         *
         * @type Point
         */
        position: {
            read: function()
            {
                return {x: this.x, y: this.y};
            }
        }
    }
});

/**
 * Motion event.
 */
Class.define('MotionEvent', {
    extend: 'PointerEvent',
    
    // Set event type.
    type: EventType.MOTION
});

/**
 * Scroll event.
 */
Class.define('ScrollEvent', {
    extend: 'PointerEvent',
    
    // Set event type.
    type: EventType.SCROLL,
    
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers, x, y, delta)
    {
        this.delta = delta;
        
        ScrollEvent.base.construct.call(this, source, modifiers, x, y);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The amount of scrolling. Positive if scrolling down, negative if scrolling up.
         *
         * @type int
         */
        delta: {
            read: true,
            defaultValue: 0
        }
    }
});

/**
 * Crossing event.
 */
Class.define('CrossingEvent', {
    extend: 'PointerEvent',
    
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers, x, y, enter, related)
    {
        this.type    = enter ? EventType.ENTER : EventType.LEAVE;
        this.related = related;
        
        CrossingEvent.base.construct.call(this, source, modifiers, x, y);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The #Widget that the pointer came from at a leave event, or went to at an enter event.
         *
         * @type Widget
         */
        related: {
            read: true,
            defaultValue: null
        }
    }
});

/**
 * Button event.
 */
Class.define('ButtonEvent', {
    extend: 'PointerEvent',
    
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers, x, y, press, button)
    {
        this.type   = press ? EventType.BUTTON_PRESS : EventType.BUTTON_RELEASE;
        this.button = button;
        
        ButtonEvent.base.construct.call(this, source, modifiers, x, y);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The button that was pressed.
         *
         * @type Button
         */
        button: {
            read: true,
            defaultValue: 0
        }
    }
});

/**
 * Key event.
 */
Class.define('KeyEvent', {
    extend: 'Event',
    
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers, press, key)
    {
        this.type = press ? EventType.KEY_PRESS : EventType.KEY_RELEASE;
        this.key  = key;
        
        KeyEvent.base.construct.call(this, source, modifiers);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The key that was pressed.
         *
         * @type Key
         */
        key: {
            read: true,
            defaultValue: 0
        }
    }
});

/**
 * Focus change event.
 */
Class.define('FocusChangeEvent', {
    extend: 'Event',
    
    /*
     * Public methods.
     */
    
    construct: function(source, modifiers, focus, related)
    {
        this.type    = focus ? EventType.FOCUS : EventType.BLUR;
        this.related = related;
        
        FocusChangeEvent.base.construct.call(this, source, modifiers);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The #Widget that gained focus at a blur event, or lost focus at a focus event.
         *
         * @type Widget
         */
        related: {
            read: true,
            defaultValue: null
        }
    }
});
