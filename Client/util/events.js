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
        type: {
            read: true,
            defaultValue: null
        },
        timestamp: {
            read: function()
            {
                return +this.date - this.date.getTimezoneOffset() * 60000;
            }
        },
        source: {
            read: true,
            defaultValue: null
        },
        modifiers: {
            read: true,
            defaultValue: EventModifierMask.NONE
        },
    },
    
    /*
     * Actions.
     */
    
    actions: {
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
        x: {
            read: true,
            defaultValue: 0
        },
        y: {
            read: true,
            defaultValue: 0
        },
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
    
    construct: function(source, modifiers, focus)
    {
        this.type = focus ? EventType.FOCUS : EventType.BLUR;
        
        FocusChangeEvent.base.construct.call(this, source, modifiers);
    }
});
