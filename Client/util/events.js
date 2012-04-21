// Use strict mode if available.
"use strict";

// Save browser event reference.
//var BrowserEvent = Event; // TODO: Not in IE.

/**
 * Event base class.
 */
Class.define('Event', {
    extend: 'Object',
    
    /*
     * Public methods.
     */
    
    construct: function(widget, modifiers)
    {
        this.date      = new Date();
        this.source    = widget;
        this.modifiers = modifiers;
    },
    
    /*
     * Properties.
     */
    
    getType: function()
    {
        return this.type;
    },
    getTimestamp: function()
    {
        return +this.date - this.date.getTimezoneOffset() * 60000;
    },
    getSource: function()
    {
        return this.source;
    },
    getModifiers: function()
    {
        return this.modifiers;
    },
    hasModifier: function(modifier)
    {
        return (this.modifiers & modifier);
    },
    
    /*
     * Internal methods.
     */
    
    // Only to be used by EventManager, to set the source.
    setSource: function(source)
    {
        this.source = source;
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
    
    getX: function()
    {
        return this.x;
    },
    getY: function()
    {
        return this.y;
    },
    getPosition: function()
    {
        return {x: this.x, y: this.y};
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
    
    getDelta: function()
    {
        return this.delta;
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
    
    getRelated: function()
    {
        return this.related;
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
    
    getButton: function()
    {
        return this.button;
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
    
    getKey: function()
    {
        return this.key;
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
