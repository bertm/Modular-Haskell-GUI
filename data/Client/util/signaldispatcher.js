// Use strict mode if available.
"use strict";

/*
 * Signal dispatcher class.
 */

Class.define('SignalDispatcher', {
    extend: 'Object',
    
    // Put them on the prototype to save memory.
    slots: {},
    blocked: {},
    
    /*
     * Public methods.
     */
    
    construct: function()
    {
    },
    
    connect: function(name, method, scope)
    {
        if (this.slots === SignalDispatcher.prototype.slots)
            this.slots = {};
        
        var handlers = this.slots[name];
        if (!handlers)
            handlers = this.slots[name] = [];
        
        handlers.splice(handlers.length - (handlers.insertOffset || 0), 0, {method: method, scope: scope});
    },
    
    connectFirst: function(name, method, scope)
    {
        if (this.slots === SignalDispatcher.prototype.slots)
            this.slots = {};
        
        var handlers = this.slots[name];
        if (!handlers)
            handlers = this.slots[name] = [];
        
        handlers.unshift({method: method, scope: scope});
    },
    
    connectLast: function(name, method, scope)
    {
        if (this.slots === SignalDispatcher.prototype.slots)
            this.slots = {};
        
        var handlers = this.slots[name];
        if (!handlers)
            handlers = this.slots[name] = [];
        
        handlers.push({method: method, scope: scope});
        handlers.insertOffset = (handlers.insertOffset || 0) + 1;
    },
    
    disconnect: function(name, method, scope)
    {
        var handlers = this.slots[name];
        if (!handlers)
            return;
        
        for (var i = handlers.length - 1; i >= 0; --i)
        {
            if ((handlers[i].method === method) && (handlers[i].scope === scope))
            {
                handlers.splice(i, 1);
                
                return;
            }
        }
    },
    
    emit: function(name)
    {
        //if (!this.blocked[name])
        //    console.info('Emitting signal \'%s\'.', name);
        
        var handlers = this.slots[name];
        if (!handlers || this.blocked[name])
            return false;
        
        var args   = Array.prototype.slice.call(arguments, 1);
        var length = handlers.length;
        
        var retVal = false;
        for (var i = 0; i < length; ++i)
        {
            var handler = handlers[i];
            
            if (handler.method.apply(handler.scope, args))
                retVal = true;
        }
        
        return retVal;
    },
    
    block: function(name)
    {
        if (this.blocked === SignalDispatcher.prototype.blocked)
            this.blocked = {};
        
        if (this.blocked[name])
            ++this.blocked[name];
        else
            this.blocked[name] = 1;
    },
    
    unblock: function(name)
    {
        if (!this.blocked[name])
            return;
        
        if (this.blocked[name] === 1)
            delete this.blocked[name];
        else
            --this.blocked[name];
    }
});
