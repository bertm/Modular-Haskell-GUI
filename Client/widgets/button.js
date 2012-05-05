// Use strict mode if available.
"use strict";

/*
 * Widget that emits a signal when clicked on.
 */
Class.define('Button', {
    extend: 'Bin',
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        Button.base.initialize.call(this);
        
        // Add events handlers.
        EventManager.registerHandler(this.el, EventMask.KEY_PRESS, this.onKeyPress, this);
        EventManager.registerHandler(this.el, EventMask.KEY_RELEASE, this.onKeyRelease, this);
        EventManager.registerHandler(this.el, EventMask.BUTTON_PRESS, this.onButtonPress, this);
        EventManager.registerHandler(this.el, EventMask.BUTTON_RELEASE, this.onButtonRelease, this);
    },
    
    getHtml: function()
    {
        var html =
            '<div tabindex="-1" class="x-widget x-button">' +
                '<div class="x-body" />' +
            '</div>';
        
        return html;
    },
    
    destroy: function()
    {
        Button.base.destroy.call(this);
        
        // TODO: Remove handlers.
    },
    
    /*
     * Signal handlers.
     */
    
    onKeyRelease: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        var key = e.getKey();
        if ((key === Key.ENTER) || (key === Key.SPACE))
        {
            this.el.removeClass('x-pressed');
        }
    },
    
    onKeyPress: function(e)
    {
        if (!this.getIsSensitive())
            return;
        
        if (this.enterDelay)
            return;
        
        var key = e.getKey();
        if (key === Key.ENTER)
        {
            this.el.addClass('x-pressed');
            
            this.enterDelay = Util.delay(100, function()
                {
                    delete this.enterDelay;
                    this.el.removeClass('x-pressed');
                }, this);
            
            // Handled it.
            return true;
        }
        else if (key === Key.SPACE)
        {
            this.el.addClass('x-pressed');
            
            // Handled it.
            return true;
        }
    },
    
    onButtonRelease: function()
    {
        this.el.removeClass('x-pressed');
        
        if (this.enterDelay)
        {
            Util.clearDelay(this.enterDelay);
            delete this.enterDelay;
        }
        
        if (!this.getIsSensitive())
            return;
        
        this.signalDispatcher.emit('activate', this);
    },
    
    onButtonPress: function()
    {
        if (!this.getIsSensitive())
            return;
        
        this.el.addClass('x-pressed');
        
        if (this.focusOnClick)
            this.focus();
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The label of the button. If set, will add a #Label to the button if it does not contain one yet.
         * If read, will return `null` if the button does not contain a label child.
         *
         * Does not signal.
         *
         * @type string
         */
        label: {
            write: function(label)
            {
                var child = this.children[0];
                if (!child)
                    this.add(new Label({label: label, 'x-align': this.xAlign, 'y-align': this.yAlign, visible: true}));
                else if (child instanceof Label)
                    child.setLabel(label || '');
                
                return false;
            },
            read: function()
            {
                var child = this.children[0];
                if (child && (child instanceof Label))
                    return child.getLabel();
                else
                    return null;
            }
        },
        /**
         * Whether the button will be receive focus on a button press.
         *
         * @type bool
         */
        'focus-on-click': { // TODO: Generalize: focus-policy
            write: function(focusOnClick)
            {
                this.focusOnClick = focusOnClick;
            },
            read: true,
            defaultValue: true
        },
        'x-align': { // TODO: label-v-align
            write: function(xAlign)
            {
                this.xAlign       = xAlign;
                this.useAlignment = true;
                
                // Set it on child.
                var child = this.children[0];
                if (child && child.hasProperty('x-align'))
                    child.setXAlign(xAlign);
                
                // TODO: What about when the child its x-align changes? What do we do? Do we change ours? Do we emit a signal?
            },
            read: true,
            defaultValue: 0.5
        },
        'y-align': { // TODO: label-h-align
            write: function(yAlign)
            {
                this.yAlign       = yAlign;
                this.useAlignment = true;
                
                // Set it on child.
                var child = this.children[0];
                if (child && child.hasProperty('y-align'))
                    child.setYAlign(yAlign);
            },
            read: true,
            defaultValue: 0.5
        },
        'use-alignment': {
            write: function(useAlignment)
            {
                this.useAlignment = useAlignment;
            },
            read: true,
            defaultValue: false
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget)' action.
        add: function(widget)
        {
            if (this.useAlignment)
            {
                if (widget.hasProperty('x-align'))
                    widget.setXAlign(this.xAlign);
                
                if (widget.hasProperty('y-align'))
                    widget.setYAlign(this.yAlign);
            }
            
            Button.base.add.call(this, widget);
        }
    }
});
