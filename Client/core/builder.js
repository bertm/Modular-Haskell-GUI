// Use strict mode if available.
"use strict";

/*
 * Builder class.
 */

Class.define('Builder', {
    /*
     * Public methods.
     */
    
    addFromFile: function(filename)
    {
        this.build(Util.loadXmlFromFile(filename));
    },
    
    /*
     * Private methods.
     */
    
    construct: function()
    {
    },
    
    build: function(document)
    {
        var rootNode = document.documentElement;
        
        if (rootNode.nodeName !== 'interface')
            throw new Error('Document does not appear to be an interface.');
        
        for (var i = 0; i < rootNode.childNodes.length; ++i)
        {
            var childNode = rootNode.childNodes[i];
            
            if (childNode.nodeName === 'object')
                this.buildObject(childNode);
        }
        
        /*
        
        XML GTK+ interface structure:
        
            interface
                requires
                
                object
                    property
                    property
                    child
                        object
                            [recurse]
                        packing
                            property
                            property
                    child
                        object
                        packing
                            property
                            property
                object
                    property
                    property
                    child
                        object
                            [recurse]
                        packing
                            property
                            property
                    child
                        object
                        packing
                            property
                            property
            
        Node attributes:
        
            - interface
            - requires: lib, version
            - object: class, id
            - property: name (text content is value)
            - child: type
        
        */
    },
    
    buildObject: function(objectNode)
    {
        // Determine class name.
        var objectClass = objectNode.getAttribute('class').replace(/^Gtk/, '');
        
        var className;
        var properties = {};
        switch (objectClass)
        {
            case 'Alignment':
            case 'AspectFrame':
            case 'Button':
            case 'Calendar':
            case 'CheckButton':
            case 'Entry':
            case 'Frame':
            case 'Fixed':
            case 'Image':
            case 'Label':
            case 'ProgressBar':
            case 'RadioButton':
            case 'ScrolledWindow':
            case 'ToggleButton':
            case 'Window':
                className = objectClass;
                break;
                
            case 'VBox':
                properties.orientation = Orientation.VERTICAL;
                
                // Fallthrough.
                
            case 'HBox':
                className = 'Box';
                break;
                
            case 'VPaned':
                properties.orientation = Orientation.VERTICAL;
                
                // Fallthrough.
                
            case 'HPaned':
                className = 'Paned';
                break;
                
            case 'VScale':
                properties.orientation = Orientation.VERTICAL;
                
                // Fallthrough.
                
            case 'HScale':
                className = 'Scale';
                break;
                
            case 'VSeparator':
                properties.orientation = Orientation.VERTICAL;
                
                // Fallthrough.
                
            case 'HSeparator':
                className = 'Separator';
                break;
                
            case 'Statusbar':
                className = 'StatusBar';
                break;
            
            default:
                throw new Error('Object of type \'' + objectClass + '\' is not supported.');
        }
        
        // Create instance.
        var obj = Instance.create(className);
        
        // Determine properties.
        for (var i = 0; i < objectNode.childNodes.length; ++i)
        {
            var propertyNode = objectNode.childNodes[i];
            
            if (propertyNode.nodeName === 'property')
            {
                var name  = propertyNode.getAttribute('name');
                var value = propertyNode.firstChild
                          ? propertyNode.firstChild.nodeValue
                          : '';
                
                name  = Builder.convertProperyName(name);
                value = Builder.convertProperyValue(value);
                
                properties[name] = value;
            }
        }
        
        // Add children.
        for (var i = 0; i < objectNode.childNodes.length; ++i)
        {
            var childNode = objectNode.childNodes[i];
            
            if (childNode.nodeName === 'child')
            {
                // Skip abnormal childs.
                // TODO: Implement this.
                if (childNode.getAttribute('type'))
                    continue;
                
                // Fetch child object node.
                var child = undefined, packingNode = undefined;
                for (var j = 0; j < childNode.childNodes.length; ++j)
                {
                    var childObjectNode = childNode.childNodes[j];
                    
                    if (childObjectNode.nodeName === 'object')
                    {
                        // Check if there already was a child object.
                        if (child)
                            throw new Error('Child has multiple objects.');
                        
                        // Build child.
                        child = this.buildObject(childObjectNode);
                    }
                    else if (childObjectNode.nodeName === 'packing')
                    {
                        packingNode = childObjectNode;
                    }
                }
                
                // Check if there is a child.
                if (!child)
                {
                    console.warn('Skipping child node without object.');
                    
                    continue;
                }
                
                // Determine packing.
                var packing = {};
                if (packingNode)
                {
                    for (var j = 0; j < packingNode.childNodes.length; ++j)
                    {
                        var propertyNode = packingNode.childNodes[j];
                        
                        if (propertyNode.nodeName === 'property')
                        {
                            var name  = propertyNode.getAttribute('name');
                            var value = propertyNode.firstChild
                                      ? propertyNode.firstChild.nodeValue
                                      : '';
                            
                            name  = Builder.convertProperyName(name);
                            value = Builder.convertProperyValue(value);
                            
                            packing[name] = value;
                        }
                    }
                }
                
                // Determine packing args.
                var args = [child];
                switch (className)
                {
                    case 'Alignment':
                    case 'Frame':
                    case 'Window':
                        // No arguments.
                        break;
                        
                    case 'Box':
                    case 'Statusbar':
                        // TODO: position.
                        
                        args.push((packing.expand !== undefined) ? packing.expand : true);
                        args.push((packing.fill   !== undefined) ? packing.fill   : true);
                        
                        if (args.padding !== undefined)
                            child.setMargin(args.padding);
                        break;
                        
                    case 'Fixed':
                        args.push(packing.x || 0);
                        args.push(packing.y || 0);
                        
                    case 'Paned':
                        // TODO: ..
                        break;
                }
                
                // Add child.
                obj.add.apply(obj, args);
            }
        }
        
        // Set properties.
        for (var name in properties)
        {
            if (obj.hasProperty(name))
            {
                obj.setProperty(name, properties[name]);
            }
            else
            {
                // Property is not supported by object.
                console.warn('Property named \'%s\' is not supported by \'%s\' widget.',
                    name, className);
            }
        }
        
        return obj;
    },
    
    statics: {
        convertProperyName: function(name)
        {
            name = name.replace(/_/g, '-');
            
            switch (name)
            {
                case 'border-width':
                    name = 'margin';
                    break;
                    
                case 'yscale':
                    name = 'y-scale';
                    break;
                    
                case 'xscale':
                    name = 'x-scale';
                    break;
                    
                case 'xalign':
                    name = 'x-align';
                    break;
                    
                case 'yalign':
                    name = 'y-align';
                    break;
                    
                case 'xpad':
                    name = 'x-padding';
                    break;
                    
                case 'ypad':
                    name = 'y-padding';
                    break;
                    
                case 'pixbuf':
                    name = 'file';
                    break;
            }
            
            return name;
        },
        
        convertProperyValue: function(value)
        {
            // TODO: Property typing, builder interface.
            
            if (value.toLowerCase() === 'true')
                return true;
            else if (value.toLowerCase() === 'false')
                return false;
            else if (value.match(/^[+-]?\d+(\.\d+)?$/))
                return Util.toNumber(value);
            else
                return value;
        }
    }
});
