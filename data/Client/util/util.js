// Use strict mode if available.
"use strict";

/*
 * Util class.
 */

Class.define('Util', { extend: 'Object', statics: {
    /*
     * Type conversion.
     */
    
    toString: function(value)
    {
        if (!value)
            return '';
        
        return value.toString();
    },
    
    toBoolean: function(value)
    {
        return !!value;
    },
    
    toInteger: function(value)
    {
        return Math.floor(value);
    },
    
    toDouble: function(value)
    {
        return +value;
    },
    
    toNumber: function(value)
    {
        return +value;
    },
    
    /*
     * Value operations.
     */
    
    clamp: function(value, minValue, maxValue)
    {
        return (value > maxValue) ? maxValue : ((value < minValue) ? minValue : value);
    },
    
    lerp: function(alpha, first, second)
    {
        return first * (1 - alpha) + second * alpha;
    },
    
    // Creates a shallow clone of the given object.
    cloneShallow: function(obj)
    {
        var newObj = {};
        for (var name in obj)
        {
            newObj[name] = obj[name];
        }
        
        return newObj;
    },
    
    // Creates a clone of the given object.
    clone: function(obj, deep)
    {
        var newObj = {};
        for (var name in obj)
        {
            if (deep && (obj[name] instanceof Object))
                newObj[name] = Util.clone(obj[name], deep);
            else
                newObj[name] = obj[name];
        }
        
        return newObj;
    },
    
    // Escapes a string for displaying.
    escapeText: function(str)
    {
        if (!str)
            return '';
        
        return str.replace(/&/g, '&amp;').
                   replace(/"/g, '&quot;').
                   replace(/</g, '&lt;').
                   replace(/>/g, '&gt;').
                   replace(/  /g, '&nbsp; ').
                   replace(/\n/g, '<br />');
    },
    
    // Replaces dash format with camelcase format.
    toCamelCase: function(value)
    {
        return value.charAt(0).toUpperCase() +
            value.slice(1).replace(/\-./g,
                function(value)
                {
                    return value.slice(1).toUpperCase();
                });
    },
    
    /*
     * Object operations.
     */
    
    apply: function(base, augment, overwrite)
    {
        if (overwrite === true)
        {
            for (var name in augment)
            {
                base[name] = augment[name];
            }
        }
        else
        {
            for (var name in augment)
            {
                if (base[name] === undefined)
                    base[name] = augment[name];
            }
        }
        
        return base;
    },
    
    /*
     * XML loading.
     */
    
    loadXmlFromFile: function(filename)
    {
        // Load file synchroniously.
        try
        {
            var time = (new Date()).getTime();
            
            var xmlHttpRequest = new XMLHttpRequest();
            xmlHttpRequest.open('GET', filename + '?time=' + time, false);
            xmlHttpRequest.send();
            
            if (xmlHttpRequest.responseXML)
                return xmlHttpRequest.responseXML;
        }
        catch (e) { }
        
        throw new Error('Failed to load XML file.');
    },
    
    /*
     * Text measuring.
     */
    
    measureTextSize: function(text, maxWidth)
    {
        var body = Element.getBody();
        
        // TODO: Copy font attributes:
        
        // font-family, font-size, font-style, font-variant, font-weight
        // line-height, word-spacing, letter-spacing, text-transform, text-indent
        
        
        // white-space: pre / pre-wrap;
        
        // TODO: Keep element.
        
        
        var el = new Element('<div class="x-text-measure" />');
        
        body.append(el);
        
        el.setText(text);
        
        var width = el.getInnerWidth();
        
        if ((maxWidth !== undefined) && (width > maxWidth))
        {
            el.setStyle('white-space', 'normal');
            el.setInnerWidth(maxWidth);
            
            width = maxWidth;
        }
        
        var height = el.getInnerHeight();
        
        el.destroy();
            
        return {width: width, height: height};
    },
    
    /*
     * Delaying and deferring methods.
     */
    
    delay: function(milliseconds, method, scope)
    {
        var args = Array.prototype.slice.call(arguments, 3);
        
        return setTimeout(function() { method.apply(scope, args); }, milliseconds);
    },
    
    defer: function(method, scope)
    {
        var args = Array.prototype.slice.call(arguments, 2);
        
        return setTimeout(function() { method.apply(scope, args); }, 0);
    },
    
    clearDelay: function(id)
    {
        clearTimeout(id);
    }
}});
