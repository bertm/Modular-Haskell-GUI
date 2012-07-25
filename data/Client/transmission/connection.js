// Use strict mode if available.
"use strict";

/*
 * Connection class.
 */

Class.define('Connection', {
    extend: 'Instance',
    
    /*
     * Public methods.
     */
    
    construct: function(host, port, path, secure)
    {
        Connection.base.construct.call(this);
        
        this.host   = host;
        this.port   = port;
        this.path   = (path ? path : '/');
        this.secure = (secure === true);
        
        this.open = this.closed = false;
        
        this.initialize();
    },
    
    send: function(data)
    {
        if (!this.open)
            throw new Error('Connection has not yet been opened.');
        
        if (this.closed)
            throw new Error('Connection has been closed.');
        
        try
        {
            this.socket.send(this.encode(data));
        }
        catch (e) { }
    },
    
    close: function()
    {
        if (this.open && !this.closed)
        {
            this.socket.close();
            this.onSocketClose();
        }
        
        this.open   = false;
        this.closed = true;
    },
    
    destroy: function()
    {
        Connection.base.destroy.call(this);
        
        this.close();
    },
    
    /*
     * Private methods; initialization.
     */
    
    initialize: function()
    {
        // Open WebSocket.
        this.socket = new WebSocket('ws' + (this.secure ? 's' : '') + '://' + this.host + ':' + this.port + this.path);
        
        // Add event handlers.
        var self = this;
        this.socket.onopen    = function()        { self.onSocketOpen();           };
        this.socket.onmessage = function(message) { self.onSocketMessage(message); };
        this.socket.onclose   = function()        { self.onSocketClose();          };
    },
    
    /*
     * Encoding and decoding.
     */
    
    encode: function(data)
    {
        try
        {
            return JSON.stringify(data);
        }
        catch (e)
        {
            // On error, throw an exception.
            throw new Error('Could not encode given data.');
        }
    },
    
    decode: function(data)
    {
        console.log('Got some raw data:');
        console.log(data);
        
        try
        {
            return JSON.parse(data);
        }
        catch (e)
        {
            // On error, directly close connection, and return null.
            this.close();
            
            return null;
        }
    },
    
    /*
     * Event handlers.
     */
    
    onSocketOpen: function()
    {
        this.open = true;
        
        this.signalDispatcher.emit('open', this);
    },
    
    onSocketMessage: function(message)
    {
        this.signalDispatcher.emit('data', this, this.decode(message.data));
    },
    
    onSocketClose: function()
    {
        if (this.closed)
            return;
        
        this.signalDispatcher.emit('close', this);
        
        this.open   = false;
        this.closed = true;
    }
});
