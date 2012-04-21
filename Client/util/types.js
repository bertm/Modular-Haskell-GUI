// Use strict mode if available.
"use strict";

// Define class.
function Class() { };

// Class agumenter.
Class.augment = function(clsName, members)
{
    var cls = window[clsName];
    
    // Add statics.
    if (members.statics !== undefined)
    {
        for (var name in members.statics)
        {
            cls[name] = members.statics[name];
        }
        
        delete members.statics;
    }
    
    // Add properties.
    if (cls.base.self && cls.base.self.properties)
    {
        cls.properties = cls.properties || Util.cloneShallow(cls.base.self.properties);
        if (members.properties !== undefined)
        {
            // Augment properties.
            for (var name in members.properties)
            {
                var property  = Util.apply(members.properties[name], cls.properties[name] || {});
                var camelCase = Util.toCamelCase(name);
                
                cls.properties[name] = property;
                
                var member = camelCase.charAt(0).toLowerCase() + camelCase.slice(1);
                
                if (property.read)
                {
                    if (property.read === true)
                    {
                        // Scope 'member' var.
                        (function(member)
                        {
                            cls.prototype['get' + camelCase] = property.read = function() { return this[member]; };
                        })(member);
                    }
                    else
                    {
                        cls.prototype['get' + camelCase] = property.read;
                    }
                }
                
                var hasDefaultValue = (property.defaultValue !== undefined);
                if (property.write)
                {
                    // Scope 'member', 'name', 'write' and 'hasDefaultValue' vars.
                    (function(member, name, write, hasDefaultValue)
                    {
                        cls.prototype['set' + camelCase] = property.write = function(value)
                        {
                            if (!hasDefaultValue || (value !== this[member]))
                            {
                                if (write.call(this, value) !== false)
                                {
                                    this.signalDispatcher.emit(name + '-change', this);
                                    this.signalDispatcher.emit('property-change', this, name);
                                    this.signalDispatcher.emit('change', this);
                                    
                                    return true;
                                }
                            }
                            
                            return false;
                        };
                    })(member, name, property.write, hasDefaultValue);
                }
                
                if (hasDefaultValue)
                    cls.prototype[member] = property.defaultValue;
            }
            
            delete members.properties;
        }
    }
    
    // Add actions.
    if (cls.base.self && cls.base.self.actions)
    {
        cls.actions = cls.actions || Util.cloneShallow(cls.base.self.actions);
        if (members.actions !== undefined)
        {
            // Augment actions.
            for (var name in members.actions)
            {
                cls.prototype[name] = cls.actions[name] = members.actions[name];
            }
            
            delete members.actions;
        }
    }
    
    // Add other members.
    for (var name in members)
    {
        cls.prototype[name] = members[name];
    }
};

// Class creator.
Class.define = function(clsName, members)
{
    // Create class definition.
    var cls = window[clsName] = function Constructor()
    {
        this.construct.apply(this, arguments);
    };
    
    // Determine base class name.
    var baseName = members.extend || 'Instance';
    delete members.extend;
    
    // Base it on base class.
    var baseCls = window[baseName];
    
    var baseClsFunc = function Constructor() { };
    baseClsFunc.prototype = baseCls.prototype;
    
    cls.prototype = new baseClsFunc;
    cls.base = baseCls.prototype;
    
    // Set self.
    cls.prototype.self = cls;
    
    // Augment class with the rest of the members.
    Class.augment(clsName, members);
};

/**
 * Base class that all classes derive from that have properties and signals.
 */
Class.define('Instance', {
    extend: 'Object',
    
    /*
     * Construction, destruction.
     */
    
    construct: function(properties)
    {
        // Create signal dispatcher.
        this.signalDispatcher = new SignalDispatcher();
        
        // Set properties.
        if (properties)
            this.setProperties(properties);
    },
    
    destroy: function()
    {
        // Check if already destroyed, and set flag.
        if (this.destroyed === true)
            throw new Error('Instance has already been destroyed.');
        
        this.destroyed = true;
        
        // Signal destruction.
        this.signalDispatcher.emit('destroy', this);
    },
    
    /*
     * Property getters and setters.
     */
    
    setProperty: function(name, value)
    {
        var property = this.self.properties[name];
        if (!property || !property.write)
            throw new Error('Instance has no writable property named \'' + name + '\'.');
        
        return property.write.call(this, value);
    },
    
    setProperties: function(properties)
    {
        // Unblock changed signal: we only want to fire it once.
        this.signalDispatcher.block('change');
        
        // Defer setting visiblity.
        var visible = properties.visible;
        delete properties.visible; // TODO: Move.
        
        // Set properties.
        var retVal = false;
        for (var name in properties)
        {
            if (this.setProperty(name, properties[name]))
                retVal = true;
        }
        
        // Set visibility.
        if (visible !== undefined)
            if (this.setVisible(visible)) // TODO: Move.
                retVal = true;
        
        // Unblock change signal and signal it.
        this.signalDispatcher.unblock('change');
        
        if (retVal)
            this.signalDispatcher.emit('change', this);
        
        return retVal;
    },
    
    getProperty: function(name)
    {
        var property = this.self.properties[name];
        if (!property || !property.read)
            throw new Error('Instance has no readable property named \'' + name + '\'.');
        
        return property.read.call(this);
    },
    
    // TODO: getProperties()
    
    hasProperty: function(name, readOnly)
    {
        var property = this.self.properties[name];
        
        return property && property.read && ((readOnly === true) || property.write);
    },
    
    /*
     * Action methods.
     */
    
    doAction: function(name, args)
    {
        var action = this.self.actions[name];
        if (!action)
            throw new Error('Object has no action named \'' + name + '\'.');
        
        return action.apply(this, args);
    },
    
    hasAction: function(name)
    {
        var action = this.self.actions[name];
        
        return !!action;
    },
    
    /*
     * Signal proxy methods.
     */
    
    connect: function(name, scope, method)
    {
        this.signalDispatcher.connect(name, scope, method);
    },
    
    connectFirst: function(name, scope, method)
    {
        this.signalDispatcher.connectFirst(name, scope, method);
    },
    
    connectLast: function(name, scope, method)
    {
        this.signalDispatcher.connectLast(name, scope, method);
    },
    
    disconnect: function(name, scope, method)
    {
        this.signalDispatcher.disconnect(name, scope, method);
    },
    
    /*
     * Statics.
     */
    
    statics: {
        properties: {},
        actions: {},
        
        // Variable instance creator. It has some overhead, so try not to use this.
        create: function(className)
        {
            var cls = window[className];
            
            var clsFunc = function Constructor() { };
            clsFunc.prototype = cls.prototype;
            
            var obj = new clsFunc;
            obj.construct.apply(obj, Array.prototype.slice.call(arguments, 1));
            
            return obj;
        },

        // Destroys an object.
        destroy: function(obj)
        {
            if (obj instanceof Array)
            {
                ;
            }
            
            // TODO: Implement.
        }
    }
});

// Define enum.
function Enum() { };

// Enumeration creator.
Enum.define = function(enumName, items)
{
    window[enumName] = items;
};

/**
 * Base class for all singleton instances.
 */
Class.define('Singleton', {
    statics: {
        properties: {},
        actions: {},
        
        define: function(clsName, members)
        {
            // Define class.
            if (!members.extend)
                members.extend = 'Singleton';
            
            delete members.statics;
            
            Class.define(clsName, members);
            
            // Create singleton instance.
            window[clsName] = new window[clsName]();
        }
    }
});
