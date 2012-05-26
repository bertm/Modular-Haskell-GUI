// Use strict mode if available.
"use strict";

/*
 * Box class.
 */

Class.define('Box', {
    extend: 'Container',
    
    /*
     * Private methods; initializing.
     */
    
    initialize: function()
    {
        Box.base.initialize.call(this);
        
        // Set members.
        this.expand = [];
    },
    
    getHtml: function()
    {
        return '<div class="x-widget x-box x-body" />';
    },
    
    /*
     * Layouting.
     */
    
    getPreferredSize: function()
    {
        var prefSize = {minimum: {width: 0, height: 0}, natural: {width: 0, height: 0}};
        
        var minSize = prefSize.minimum;
        var natSize = prefSize.natural;
        
        var horizontal = (this.orientation === Orientation.HORIZONTAL);
        
        var nrVisibleChildren = 0;
        for (var i = this.children.length - 1; i >= 0; --i)
        {
            var child = this.children[i];
            if (child.visible)
            {
                ++nrVisibleChildren;
                
                var size = child.getSizeRequisition();
                
                if (horizontal)
                {
                    minSize.height = Math.max(minSize.height, size.minimum.height);
                    natSize.height = Math.max(natSize.height, size.natural.height);
                    
                    if (this.homogeneous)
                    {
                        minSize.width = Math.max(minSize.width, size.minimum.width);
                        natSize.width = Math.max(natSize.width, size.natural.width);
                    }
                    else
                    {
                        minSize.width += size.minimum.width;
                        natSize.width += size.natural.width;
                    }
                }
                else
                {
                    minSize.width = Math.max(minSize.width, size.minimum.width);
                    natSize.width = Math.max(natSize.width, size.natural.width);
                    
                    if (this.homogeneous)
                    {
                        minSize.height = Math.max(minSize.height, size.minimum.height);
                        natSize.height = Math.max(natSize.height, size.natural.height);
                    }
                    else
                    {
                        minSize.height += size.minimum.height;
                        natSize.height += size.natural.height;
                    }
                }
            }
        }
        
        if (nrVisibleChildren > 0)
        {
            if (this.homogeneous)
            {
                if (horizontal)
                {
                    minSize.width *= nrVisibleChildren;
                    natSize.width *= nrVisibleChildren;
                }
                else
                {
                    minSize.height *= nrVisibleChildren;
                    natSize.height *= nrVisibleChildren;
                }
            }
            
            var spacing = (nrVisibleChildren - 1) * this.spacing;
            if (horizontal)
            {
                minSize.width += spacing
                natSize.width += spacing;
            }
            else
            {
                minSize.height += spacing;
                natSize.height += spacing;
            }
        }
        
        return prefSize;
    },
    
    allocateSize: function(allocation)
    {
        // Correct and store allocation.
        this.correctAndStoreAllocation(allocation);
        
        // Create a handy shortcut.
        var horizontal = (this.orientation === Orientation.HORIZONTAL);
        
        // Determine amount of visible and expand children. Fetch their sizes as well.
        var sizes = [], nrVisibleChildren = 0, nrExpandChildren = 0, length = this.children.length;
        for (var i = 0; i < length; ++i)
        {
            var child = this.children[i];
            if (child.visible)
            {
                ++nrVisibleChildren;
                
                if (this.expand[i])
                    ++nrExpandChildren;
                
                // Fetch size.
                if (!this.homogeneous)
                {
                    var req = child.getSizeRequisition();
                    if (horizontal)
                        sizes.push({minimum: req.minimum.width, natural: req.natural.width});
                    else
                        sizes.push({minimum: req.minimum.height, natural: req.natural.height});
                }
            }
        }
        
        if (!nrVisibleChildren)
            return;
        
        // Determine size.
        var size, extra, rest;
        if (this.homogeneous)
        {
            if (horizontal)
                size = allocation.width - (nrVisibleChildren - 1) * this.spacing;
            else
                size = allocation.height - (nrVisibleChildren - 1) * this.spacing;
            
            extra = Math.floor(size / nrVisibleChildren);
            rest  = fullSize - extra * nrVisibleChildren;
        }
        else 
        {
            // Determine minimum size.
            var frame   = this.getFrameSize();
            var reqSize = this.getSizeRequisition().minimum;
            var minSize = horizontal ? (reqSize.width - frame.width) : (reqSize.height - frame.height);
            
            // Sort child indices by gap.
            var indices = [];
            for (var i = 0; i < nrVisibleChildren; ++i)
            {
                indices.push(i);
            }
            
            indices.sort(function(a, b)
                {
                    return (sizes[b].natural - sizes[b].minimum) - (sizes[a].natural - sizes[a].minimum);
                });
            
            console.log(indices);
            console.log(sizes);
            
            // Determine leftover space.
            if (horizontal)
                var leftover = allocation.width - minSize;
            else
                var leftover = allocation.height - minSize;
            
            // Distribute leftover space. Thanks to Behdad Esfahbod.
            for (i = nrVisibleChildren - 1; (leftover > 0) && (i >= 0); --i)
            {
                var index = indices[i];
                var glue  = Math.floor((leftover + i) / (i + 1)); // NOTE: Why +i?
                var gap   = sizes[index].natural - sizes[index].minimum;

                var add = Math.min(glue, gap);

                sizes[index].minimum += add;

                leftover -= add;
            }
            
            // Use the rest for expanding children.
            if (nrExpandChildren > 0)
            {
                extra = Math.floor(leftover / nrExpandChildren);
                rest  = leftover - extra * nrExpandChildren;
            }
            else
            {
                extra = rest = 0;
            }
        }
        
        // Setup child allocation.
        var padding = this.bodyEl.getPadding();
        
        var x = padding.left, y = padding.top;
        
        // Give each child his size.
        for (var i = 0; i < length; ++i)
        {
            // Get child, and check if it's visible.
            var child = this.children[i];
            if (!child.visible)
                continue;
            
            // Calculate child its size.
            if (this.homogeneous)
            {
                var childSize = extra;
                
                if (rest)
                {
                    ++childSize;
                    --rest;
                }
            }
            else
            {
                var childSize = sizes[i].minimum;
                
                if (this.expand[i])
                {
                    childSize += extra;
                    
                    if (rest)
                    {
                        ++childSize;
                        --rest;
                    }
                }
            }
            
            // Determine child allocation.
            var childAllocation = {x: x, y: y};
            
            if (horizontal)
            {
                childAllocation.height = allocation.height;
                childAllocation.width  = childSize;
            }
            else
            {
                childAllocation.width  = allocation.width;
                childAllocation.height = childSize;
            }
            
            // Allocate size for child.
            child.allocateSize(childAllocation);
            
            // Increase location.
            if (horizontal)
                x += childSize + this.spacing;
            else
                y += childSize + this.spacing;
        }
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The orientation of the box.
         *
         * @type Orientation
         */
        orientation: {
            write: function(orientation)
            {
                this.orientation = orientation;
                
                this.layout();
            },
            read: true,
            defaultValue: Orientation.HORIZONTAL
        },
        /**
         * Whether the space is evenly divided between the children.
         *
         * @type bool
         */
        homogeneous: {
            write: function(homogeneous)
            {
                this.homogeneous = homogeneous;
                
                this.layout();
            },
            read: true,
            defaultValue: false
        },
        /**
         * Space between each child.
         *
         * @type int
         */
        spacing: {
            write: function(spacing)
            {
                this.spacing = spacing;
                
                this.layout();
            },
            read: true,
            defaultValue: 0
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        // Overrides 'add(widget)' action.
        /**
         * Adds a widget to the box.
         *
         * @param widget Widget Widget to add.
         */
        add: function(widget, expand) // TODO: Remove expand (expand)
        {
            Box.base.add.call(this, widget);
            
            this.expand.push(expand !== false);
            
            this.layout();
        },
        // Overrides 'remove(widget)' action.
        remove: function(widget)
        {
            var index = Box.base.remove.call(this, widget);
            
            this.expand.splice(index, 1);
            
            this.layout();
        }
    }
});
