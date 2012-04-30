// Use strict mode if available.
"use strict";

Enum.define('MessageType', {
    ESTABLISH: 'establish',
    ACKNOWLEGDE: 'acknowledge',
    CREATE: 'create',
    ACTION: 'action',
    SIGNAL: 'signal',
    SET: 'set',
    KEEPALIVE: 'keepalive',
    CLOSE: 'close',
    ERROR: 'error'
});

Enum.define('TransmissionState', {
    CONNECTING: 1,
    CONNECTED: 2,
    ESTABLISHED: 3,
    CLOSED: 4
});

Singleton.define('Transmission', {
    initialize: function()
    {
        // Set our version.
        this.version = '1.0';
        
        // Set state and objects.
        this.state = TransmissionState.CONNECTING;
        
        this.singletons = {
            1: Application,
            2: Screen
        };
        
        this.objects = {};
        
        this.classes = {
            'AspectFrame': true,
            'Box': true,
            'Button': true,
            'CheckButton': true,
            'Entry': true,
            'Fixed': true,
            'Frame': true,
            'Image': true,
            'Label': true,
            'MainWindow': true,
            'Menu': true,
            'MenuBar': true,
            'MenuItem': true,
            'ProgressBar': true,
            'RadioButton': true,
            'Scale': true,
            'ScrollBar': true,
            'ScrolledWindow': true,
            'Separator': true,
            'SeparatorMenuItem': true,
            'Spinner': true,
            'StatusBar': true,
            'ToggleButton': true,
            'Window': true,
            
            'Adjustment': true,
            'RadioButtonGroup': true
        };
        
        console.log('Connecting to ' + window.location.hostname + ':' + 9000 + '.');
        
        // Create new connection.
        this.conn = new Connection(window.location.hostname, 9000);

        this.conn.connect('open', this.onConnectionOpen, this);
        this.conn.connect('close', this.onConnectionClose, this);
        this.conn.connect('data', this.onConnectionData, this);
        
        // Give singletons an id, and listen for their property changes.
        for (var id in this.singletons)
        {
            var singleton = this.singletons[id];
            
            singleton._id = +id;
            
            // Register propert-change signal handler.
            singleton.connect('property-change', this.onObjectPropertyChange, this);
        }
    },
    
    /*
     * Message handlers.
     */
    
    handleMessage: function(message)
    {
        if (message instanceof Array)
        {
            message.forEach(this.handleMessage, this);
            return;
        }
        
        console.log('Handling message:');
        console.log(message);
        
        try
        {
            if (message instanceof Object)
            {
                // Call handler.
                var handler = this.messageHandlers[message.type];
                if (handler && handler.call(this, message))
                    return;
                
                // Handler did not succeed.
                this.closeReason = 'Message handler did not succeed, or unknown message.';
            }
        }
        catch (e)
        {
            // Set close reason.
            this.closeReason = e.message;
        }
        
        // Close connection.
        this.conn.close();
    },
    
    messageHandlers: (function()
        {
            var handlers = {};
            
            handlers[MessageType.ACKNOWLEGDE] = function(message)
            {
                console.log('Got a acknowledge message.');
                
                if (this.state === TransmissionState.CONNECTED)
                {
                    if (message.version >= this.version)
                    {
                        this.serverVersion = message.version;
                        this.state = TransmissionState.ESTABLISHED;
                        
                        console.log('Established connection.');
                        
                        return true;
                    }
                    else
                    {
                        throw new Error('Server its protocol version is lower than ours.');
                    }
                }
                
                throw new Error('Got an unexpected acknowledge messsage.');
            };
            
            handlers[MessageType.CREATE] = function(message)
            {
                console.log('Got a create message.');
                
                if (this.state !== TransmissionState.ESTABLISHED)
                    throw new Error('Got a create message, while connection not established.');
                
                var cls  = message['class'];
                var id   = message.id;
                
                if (!this.classes[cls])
                    throw new Error('Class \'' + cls + '\' is not allowed to be instanciated.');
                
                if (id < 1000)
                    throw new Error('Cannot instantiate a singleton.');
                
                if (this.objects[id])
                    throw new Error('Object with id ' + id + ' already exists.');
                
                // Create object. Set id on it before construction.
                var cls = window[cls];
                
                var clsFunc = function Constructor() { };
                clsFunc.prototype = cls.prototype;
                
                var obj = this.objects[id] = new clsFunc;
                obj._id = id;
                
                obj.construct();
                
                // Register propert-change signal handler.
                obj.connect('property-change', this.onObjectPropertyChange, this);
                
                return true;
            };
            
            handlers[MessageType.ACTION] = function(message)
            {
                console.log('Got an action message.');
                
                if (this.state !== TransmissionState.ESTABLISHED)
                    throw new Error('Got an action message, while connection not established.');
                
                var id   = message.id;
                var name = message.name;
                var args = message.args;
                
                var obj = this.getObjectById(id);
                
                if (!obj.hasAction(name))
                    throw new Error('Object does not have an action named \'' + name + '\'.');
                
                // Call action.
                obj.doAction(name, this.decodeArguments(args));
                
                return true;
            };
            
            handlers[MessageType.SET] = function(message)
            {
                console.log('Got a set message.');
                
                if (this.state !== TransmissionState.ESTABLISHED)
                    throw new Error('Got a set message, while connection not established.');
                
                var id    = message.id;
                var name  = message.name;
                var value = message.value;
                
                var obj = this.getObjectById(id);
                
                if (!obj.hasProperty(name))
                    throw new Error('Object does not have a property named \'' + name + '\'.');
                
                // Set property.
                this.settingProperty = name;
                this.settingObject   = obj;
                
                obj.setProperty(name, value);
                
                delete this.settingProperty;
                delete this.settingObject;
                
                return true;
            };
            
            handlers[MessageType.CLOSE] = function(message)
            {
                console.log('Got a close message.');
                
                throw new Error('Serverside closed connection.');
            };
            
            handlers[MessageType.ERROR] = function(message)
            {
                console.log('Got an error message.');
                
                throw new Error("A serverside error occurred:\n" + message.msg);
            };
            
            return handlers;
        })(),
    
    /*
     * Helper methods.
     */
    
    getObjectById: function(id)
    {
        id = +id;
        
        if (id < 1000)
        {
            if (this.singletons[id])
                return this.singletons[id];
        }
        else
        {
            if (this.objects[id])
                return this.objects[id];
        }
        
        throw new Error('Object with id ' + id + ' could not be found.');
    },
    
    decodeArguments: function(args)
    {
        if (!(args instanceof Array))
            throw new Error('Arguments is not an array.');
        
        var result = [];
        var length = args.length;
        for (var i = 0; i < length; ++i)
        {
            result.push(this.decodeArgument(args[i]));
        }
        
        return result;
    },
    
    decodeArgument: function(arg)
    {
        if (arg instanceof Object)
        {
            // Decode array.
            if (arg instanceof Array)
                return this.decodeArguments(arg);
            
            // It's an object, get its corresponding object.
            return this.getObjectById(arg.id);
        }
        
        // A string, a number, a boolean or null.
        // Undefined and NaN is not part of JSON.
        return arg;
    },
    
    encodeArguments: function(args)
    {
        if (!(args instanceof Array))
            throw new Error('Arguments is not an array.');
        
        var result = [];
        var length = args.length;
        for (var i = 0; i < length; ++i)
        {
            result.push(this.encodeArgument(args[i]));
        }
        
        return result;
    },
    
    encodeArgument: function(arg)
    {
        if (arg instanceof Object)
        {
            // Encode array.
            if (arg instanceof Array)
                return this.encodeArguments(arg);
            
            // Check for a known object.
            if (arg._id !== undefined)
                return {id: arg._id};
            
            // TODO: Check for an instance that has been created on the client.
            // TODO: Check if it occurs in the classes list.
            // TODO: Send create message to the server.
            // NOTE: Use minus ids for those?
            
            throw new Error('Got an unexpected object.');
        }
        
        if ((arg === undefined) || (arg === NaN))
            throw new Error('Got an unexpected value.');
        
        // A string, a number, a boolean or null.
        return arg;
    },
    
    /*
     * Event handlers.
     */
    
    onConnectionOpen: function(conn)
    {
        console.log('Connection has been openened.');
        
        // Set state.
        this.state = TransmissionState.CONNECTED;
        
        // Send an establish message.
        conn.send({
            type: MessageType.ESTABLISH,
            version: this.version
        });
    },
    
    onConnectionData: function(conn, data)
    {
        console.log('Got some data:');
        console.log(data);
        
        this.handleMessage(data);
    },
    
    onConnectionClose: function(conn)
    {
        // Set reason if there is none.
        if (!this.closeReason)
        {
            if (this.state === TransmissionState.CONNECTING)
                this.closeReason = 'Could not connect to server.';
            else
                this.closeReason = 'Lost connection with server.';
        }
        
        // Show close reason.
        console.log("Connection closed. Reason:\n" + this.closeReason);
        
        this.state = TransmissionState.CLOSED;
        
        // Destroy all objects.
        for (var id in this.objects)
        {
            this.objects[id].destroy();
        }
        
        this.objects = {};
        
        // TODO: Remove event listeners from singletons.
        
        // Show a nice message.
        var window = new Window({
            margin: 15,
            deletable: false,
            resizable: false,
            maximizable: false,
            modal: true,
            title: "An error occurred"
        });
        var label = new Label({label: this.closeReason});
        
        window.add(label);
        window.showAll();
    },
    
    onObjectPropertyChange: function(obj, name)
    {
        // Stop if connection has been closed.
        if (this.state !== TransmissionState.ESTABLISHED)
            return;
        
        // Skip signals that came from our own setter.
        if (this.settingProperty && (this.settingProperty === name) && (this.settingObject === obj))
            return;
        
        console.log('A property has changed: ' + name);
        
        try
        {
            var id    = obj._id;
            var value = this.encodeArgument(obj.getProperty(name)); // TODO: Give value with it? Too much overhead?
        }
        catch (e)
        {
            this.closeReason = e.message;
            this.conn.close();
            
            return;
        }
        
        console.log('Sending property: ' + name + ', with value:');
        console.log(value);
        
        // Send a set message.
        this.conn.send({
            type: MessageType.SET,
            id: id,
            name: name,
            value: value
        });
    }
});

Transmission.initialize();
