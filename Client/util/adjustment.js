// Use strict mode if available.
"use strict";

/**
 * A representation of an adjustable bounded value.
 *
 * The adjustment object represents a value which has an associated lower and upper bound, together with step
 * and page increments, and a page size. It is used within several widgets, including #SpinButton, #ScrollBar
 * and #Scale.
 *
 * An example:
 *
 *     var adj = new Adjustment({#lower: 0, #upper: 100, #value: 75});
 *     var scale = new #Scale({adjustment: adj});
 */
Class.define('Adjustment', {
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The value of the adjustment. It will automatically be clamped between #lower and `#upper - #page-size`.
         *
         * @type float
         */
        value: {
            write: function(value)
            {
                value = Util.clamp(value, this.lower, Math.max(this.lower, this.upper - this.pageSize));
                
                if (value !== this.value)
                {
                    this.value = value;
                    
                    return;
                }
                
                // Do not signal.
                return false;
            },
            read: true,
            defaultValue: 0
        },
        /**
         * The minimum value of the adjustment.
         *
         * @type float
         */
        lower: {
            write: function(lower)
            {
                this.lower = lower;
                
                this.signalDispatcher.block('change');
                
                if (lower > this.upper)
                    this.setUpper(lower);
                
                if (this.value < lower)
                    this.setValue(lower);
                
                this.signalDispatcher.unblock('change');
            },
            read: true,
            defaultValue: 0
        },
        /**
         * The maximum value of the adjustment.
         *
         * The maximum value of the adjustment. Note that values will be restricted by `#upper - #page-size` if
         * the #page-size property is nonzero.
         *
         * @type float
         */
        upper: {
            write: function(upper)
            {
                this.upper = upper;
                
                this.signalDispatcher.block('change');
                
                if (upper < this.lower)
                    this.setLower(upper);
                
                var maxValue = Math.max(this.lower, upper - this.pageSize);
                if (this.value > maxValue)
                    this.setValue(maxValue);
                
                this.signalDispatcher.unblock('change');
            },
            read: true,
            defaultValue: 0
        },
        /**
         * The step increment of the adjustment.
         *
         * @type float
         */
        'step-increment': {
            write: function(stepIncrement)
            {
                stepIncrement = (stepIncrement < 0) ? 0 : stepIncrement;
                
                if (stepIncrement !== this.stepIncrement)
                {
                    this.stepIncrement = stepIncrement;
                    
                    return;
                }
                
                // Do not signal.
                return false;
            },
            read: true,
            defaultValue: 1
        },
        /**
         * The page increment of the adjustment.
         *
         * @type float
         */
        'page-increment': {
            write: function(pageIncrement)
            {
                pageIncrement = (pageIncrement < 0) ? 0 : pageIncrement;
                
                if (pageIncrement !== this.pageIncrement)
                {
                    this.pageIncrement = pageIncrement;
                    
                    return;
                }
                
                // Do not signal.
                return false;
            },
            read: true,
            defaultValue: 10
        },
        /**
         * The page size increment of the adjustment.
         *
         * The page size increment of the adjustment. Note that the page-size is irrelevant and should be set to zero
         * if the adjustment is used for a simple scalar value, e.g. in a #Scale.
         *
         * @type float
         */
        'page-size': {
            write: function(pageSize)
            {
                pageSize = (pageSize < 0) ? 0 : pageSize;
                
                if (pageSize !== this.pageSize)
                {
                    this.pageSize = pageSize;
                    
                    if (this.value > Math.max(this.lower, this.upper - pageSize))
                        this.setValue(this.upper - pageSize);
                    
                    return;
                }
                
                // Do not signal.
                return false;
            },
            read: true,
            defaultValue: 0
        }
    },
    
    /*
     * Actions.
     */
    
    actions: {
        /**
         * Increments the value by the #step-increment.
         */
        increment: function()
        {
            this.setValue(this.value + this.stepIncrement);
        },
        /**
         * Decrements the value by the #step-increment.
         */
        decrement: function()
        {
            this.setValue(this.value - this.stepIncrement);
        },
        /**
         * Increments the value by the #page-increment.
         */
        incrementPage: function()
        {
            this.setValue(this.value + this.pageIncrement);
        },
        /**
         * Decrements the value by the #page-increment.
         */
        decrementPage: function()
        {
            this.setValue(this.value - this.pageIncrement);
        },
        /**
         * Sets the fraction (a value from 0.0 to 1.0) of the value.
         *
         * Sets the fraction (a value from 0.0 to 1.0) of the value. The fraction `0.0` will set a value
         * of #lower and `1.0` of `#upper - #page-size`.
         */
        setFraction: function(fraction) // TODO: fraction property? Has the overhead of signals.
        {
            this.setValue(fraction * (this.upper - this.pageSize - this.lower) + this.lower);
        },
        /**
         * Gets the fraction of the value. See #setFraction() for more details.
         */
        getFraction: function()
        {
            if ((this.upper - this.pageSize - this.lower) === 0)
                return 0;
            
            return (this.value - this.lower) / (this.upper - this.pageSize - this.lower);
        }
    }
});
