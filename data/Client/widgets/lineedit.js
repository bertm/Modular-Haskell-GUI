// Use strict mode if available.
"use strict";

/**
 * The line edit widget is a one-line text editor.
 */
Class.define('LineEdit', {
    extend: 'Widget',
    
    /*
     * Private methods; initialization.
     */
    
    // NOTE: invisible char: 0xE2 0x97 0x8F (Black circle, UTF 32 character number: 9679)
    
    initialize: function()
    {
        LineEdit.base.initialize.call(this);
        
        // Set vertical scale.
        this.setVScale(0);
        
        // Fetch input element.
        this.inputEl = this.el.find('input');
        
        // Attach event handlers.
        this.inputEl.connect('change', this.onInputEvent, this);
        this.inputEl.connect('paste', this.onInputEvent, this);
        
        EventManager.registerHandler(this.inputEl, EventMask.BUTTON_PRESS, this.onButtonPress, this);
        EventManager.registerHandler(this.inputEl, EventMask.KEY_RELEASE | EventMask.BUTTON_RELEASE, this.onInputEvent, this);
    },
    
    getHtml: function()
    {
        var html =
            '<div class="x-widget x-line-edit x-shadow-in">' +
                '<input autocomplete="off" value="" />' +
            '</div>';
        
        return html;
    },
    
    /*
     * Layouting.
     */
    
    getMinimumSize: function()
    {
        return {width: 15, height: 22};
    },
    
    getNaturalSize: function()
    {
        return {width: 150, height: 22};
    },
    
    /*
     * Event handlers.
     */
    
    onButtonPress: function()
    {
        this.focus();
        
        this.onInputEvent();
    },
    
    onInputEvent: function()
    {
        // Set new input text.
        this.setText(this.inputEl.getProperty('value'));
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The entry its value.
         *
         * @type string
         */
        text: {
            write: function(text)
            {
                if (this.inputEl.getProperty('value') !== text)
                    this.inputEl.setProperty('value', text);
                
                this.text = text;
            },
            read: true,
            defaultValue: ''
        },
        /**
         * Whether the #text can be edited.
         *
         * @type bool
         */
        editable: {
            write: function(editable)
            {
                // TODO: What about sensitive? insensitive --> ineditable.
                
                editable ? this.el.removeClass('x-disabled') : this.el.addClass('x-disabled');
                
                this.inputEl.setProperty('disabled', !editable);
                
                this.editable = editable;
            },
            read: true,
            defaultValue: true
        },
        /**
         * The shadow type of the entry.
         *
         * @type ShadowType
         */
        'shadow-type': {
            write: function(shadowType)
            {
                this.el.replaceClass('x-shadow-' + this.shadowType, 'x-shadow-' + shadowType);
                
                this.shadowType = shadowType;
                
                this.layout();
            },
            read: true,
            defaultValue: ShadowType.ETCHED_IN
        },
        /**
         * Whether the entry has a shadow around it.
         *
         * @type bool
         */
        'has-frame': {
            write: function(hasFrame)
            {
                hasFrame ? this.el.removeClass('x-no-frame') : this.el.addClass('x-no-frame');
                
                this.hasFrame = hasFrame;
                
                this.layout();
            },
            read: true,
            defaultValue: true
        },
        /**
         * Whether the entered #text is shown. If `false`, the entry will be in password mode, showing each charactere
         * as a dot.
         *
         * @type bool
         */
        visibility: {
            write: function(visibility)
            {
                //visibility ? this.inputEl.setProperty('type', 'text') : this.inputEl.setProperty('type', 'password');
                
                // TODO: IE Does not like this: it needs the input element to be replaced.
                
                this.visibility = visibility;
            },
            read: true,
            defaultValue: true
        },
        /**
         * The maximum length of the entry. If set to `0`, no limit will be set.
         *
         * @type int
         */
        'max-length': {
            write: function(maxLength)
            {
                this.inputEl.setProperty('max-length', maxLength);
                
                // TODO: Check.
                
                this.maxLength = maxLength;
            },
            read: true,
            defaultValue: 0
        }
    }
});
