// Use strict mode if available.
"use strict";

/**
 * Cursor class. Manages a stack of cursors.
 *
 * See #StatusBar for an explaination of stacks and the role of contexts. The same
 * idea applies to the cursor.
 */
Singleton.define('Cursor', {
    /*
     * Private methods.
     */
    
    initialize: function()
    {
        // Set members.
        this.contexts    = [{description: 'default'}];
        this.shapes      = [];
        this.nextShapeId = 1;
    },
    
    checkContextId: function(contextId)
    {
        contextId = contextId || 0;
        if (!this.contexts[contextId])
            throw new Error('Given context id is unknown.');
        
        return contextId;
    },
    
    updateShape: function()
    {
        // Store old shape.
        var oldShape = this.shape;
        
        // Set new shape.
        var length = this.shapes.length;
        if (!length)
            this.shape = CursorShape.ARROW;
        else
            this.shape = this.shapes[length - 1].shape;
        
        // Set new shape.
        if (oldShape !== this.shape)
        {
            Element.getBody().replaceClass('x-cursor-' + oldShape, 'x-cursor-' + this.shape);
            
            this.emitPropertyChangeSignals('shape');
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The current shape of the cursor. Use #push() to change the cursor.
         */
        shape: {
            read: function()
            {
                return this.shape;
            },
            defaultValue: CursorShape.ARROW
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        /**
         * Returns a new context identifier, given a description of the actual context.
         * Note that the description is not shown in the UI.
         *
         * @param string description Description of the context.
         *
         * @return int An identifier of the context, to be used by the other actions.
         */
        getContextId: function(description)
        {
            for (var i = this.contexts.length - 1; i >= 0; --i)
            {
                if (this.contexts[i].description === description)
                    return i;
            }
            
            this.contexts.push({description: description});
            
            return this.contexts.length - 1;
        },
        /**
         * Pushes a new shape onto a cursor shape stack.
         *
         * @param CursorShape shape     Shape to be pushed on the stack.
         * @param int         contextId Context id. See #getContextId().
         *
         * @return int The id of the shape. Can be used as an identifier for #remove().
         */
        push: function(shape, contextId)
        {
            // Check context id.
            contextId = this.checkContextId(contextId);
            
            // Push shape.
            this.shapes.push({shape: shape, contextId: contextId, id: this.nextShapeId});
            ++this.nextShapeId;
            
            // Update shape.
            this.updateShape();
            
            return this.nextShapeId - 1;
        },
        /**
         * Removes the first shape from the cursor shape stack with the given context id.
         *
         * @param int contextId Context id. See #getContextId().
         */
        pop: function(contextId)
        {
            // Check context id.
            contextId = this.checkContextId(contextId);
            
            // Pop shape.
            for (var i = this.shapes.length - 1; i >= 0; --i)
            {
                if (this.shapes[i].contextId === contextId)
                {
                    this.shapes.splice(i, 1);
                    
                    // Update shape.
                    this.updateShape();
                    
                    return;
                }
            }
        },
        /**
         * Forces the removal of a shape from a cursor shape stack.
         * The exact context id and shape id must be specified.
         *
         * @param int contextId Context id of the shape to be removed. See #getContextId().
         * @param int shapeId   Shape id to be removed. See #push().
         */
        remove: function(contextId, shapeId)
        {
            // Check context id.
            contextId = this.checkContextId(contextId);
            
            // Remove shape.
            for (var i = this.shapes.length - 1; i >= 0; --i)
            {
                var shape = this.shapes[i];
                
                if ((shape.id === shapeId) && (shape.contextId === contextId))
                {
                    this.shapes.splice(i, 1);
                    
                    // Update shape.
                    this.updateShape();
                    
                    return;
                }
            }
        },
        /**
         * Forces the removal of all shapes from the cursor shape stack with the exact context id.
         *
         * @param int contextId Context id to remove shapes of. See #getContextId().
         */
        removeAll: function(contextId)
        {
            // Check context id.
            contextId = this.checkContextId(contextId);
            
            // Remove shapes with given context id.
            for (var i = this.shapes.length - 1; i >= 0; --i)
            {
                if (this.shapes[i].contextId === contextId)
                {
                    this.shapes.splice(i, 1);
                }
            }
            
            // Update shape.
            this.updateShape();
        }
    }
});
