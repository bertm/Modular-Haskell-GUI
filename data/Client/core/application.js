// Use strict mode if available.
"use strict";

/**
 * The application class manages the GUI application's control flow and main settings.
 */
Singleton.define('Application', {
    /*
     * Private methods.
     */
    
    initialize: function()
    {
        // Initialize other singletons.
        Cursor.initialize();
        Screen.initialize();
        EventManager.initialize();
        
        // Add body classes.
        //Element.getBody().addClass('x-ie');
        
        //this.showSomeStuff();
    },
    
    /*
     * Internal methods.
     */
    
    setMainWindow: function(mainWindow)
    {
        if (this.mainWindow !== mainWindow)
        {
            this.mainWindow = mainWindow;
            
            this.emitPropertyChangeSignals('main-window');
        }
    },
    
    // Will be called by Widget if focus widget has really changed.
    setRealFocusWidget: function(widget)
    {
        if (this.focusWidget !== widget)
        {
            this.focusWidget = widget;
            
            this.emitPropertyChangeSignals('focus-widget');
        }
    },
    
    // Called by the event manager when an event happens.
    onEvent: function(e, data, capturePhase)
    {
        var prefix = (capturePhase ? 'capture-' : '');
        
        return this.signalDispatcher.emit(prefix + e.type + '-event', this, e) ||
               this.signalDispatcher.emit(prefix + 'event', this, e);
    },
    
    /*
     * Properties.
     */
    
    properties: {
        /**
         * The currently active window. Set to null to deactive the currenly active window.
         *
         * @type AbtractWindow
         */
        'active-window': {
            write: function(activeWindow)
            {
                if (this.activeWindow)
                    this.activeWindow.setActive(false);
                
                if (activeWindow)
                    activeWindow.setActive(true);
                
                this.activeWindow = activeWindow;
            },
            read: true,
            defaultValue: null
        },
        /**
         * The main window of the application. May be `null`.
         *
         * @type MainWindow
         */
        'main-window': {
            read: true,
            defaultValue: null
        },
        /**
         * The widget that has the global input focus. May be `null`.
         *
         * @type Widget
         */
        'focus-widget': {
            write: function(focusWidget)
            {
                if (focusWidget)
                    focusWidget.setHasFocus(true);
                else if (this.focusWidget)
                    this.focusWidget.setHasFocus(false);
                
                // setRealFocusWidget() will be called if it really changed.
                return false;
            },
            read: true,
            defaultValue: null
        },
        /**
         * Event mask that determines which event signals are enabled.
         *
         * @type int
         * @see #EventMask
         */
        'events': {
            write: function(events)
            {
                // Register given event types.
                EventManager.registerHandler(Element.getBody(), events, this.onEvent, this);
                
                // Set events.
                this.events = events;
            },
            read: true,
            defaultValue: EventMask.NONE
        }
    },
    
    /*
     * Actions.
     */
    
    //..
    
    
    
    
    
    
    
    
    
    
    
    
    
    showSomeStuff: function()
    {
        
        //*
        //var button = new RadioButtonView;
        //this.body.append(button);
        
        var window = new MainWindow({
            title: 'Test window',
            margin: 10
            //sensitive: false
            //modal: true
            //opacity: 0.7
        });
        
        window.setEvents(window.getEvents() | EventMask.ENTER | EventMask.LEAVE);
        
        
        var progressBar = new ProgressBar({
            fraction: 0.5,
            text: '50%',
            'show-text': true,
            orientation: Orientation.VERTICAL,
            inverted: false
        });
        
        progressBar.setEvents(progressBar.getEvents() | EventMask.ENTER | EventMask.LEAVE);
        
        progressBar.setShowTooltip(true);
        progressBar.setTooltipLabel('Hi there.');
        
        var otherProgressBar = new ProgressBar({
            fraction: 0.5,
            text: '50%',
            'show-text': true,
            orientation: Orientation.VERTICAL,
            inverted: true
        });
        
        var newProgressBar = new ProgressBar({
            fraction: 0.5,
            text: '50%',
            'show-text': true,
            orientation: Orientation.HORIZONTAL,
            inverted: false
        });
        
        var aNewProgressBar = new ProgressBar({
            fraction: 0.5,
            text: '50%',
            'show-text': true,
            orientation: Orientation.HORIZONTAL,
            inverted: true
        });
        
        var box = new Box({
            orientation: Orientation.VERTICAL,
            homogeneous: false
        });
        
        var anotherBox = new Box({
            spacing: 10,
            'margin-left': 10,
            'margin-right': 10,
            orientation: Orientation.VERTICAL
        });
        
        var yetAnotherBox = new Box({
            spacing: 10
        });
        
        var frame = new Frame({
            label: 'Blah'
        });
        
        var button = new ToggleButton({
            label: 'test',
            'can-focus': true
        });
        
        var firstLabel = new Label({
            text:  'This is a good enough length for any line to have. ' +
                   'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed blandito dictum massa at varius. ' +
                   'Aenean massa leo, dapibusa et dictum ac, elementum nec elit. Donec turpis massa, cursus ut vol' +
                   'utpat eget, suscipit sit amet massa. Nullam eleifend nulla ut quam fringilla auctor. Suspendis' +
                   'se tempus, mauris a aliquet tristique, dui nunc fringilla erat, in pretium purus risus ut enim' +
                   '. Nullam eget augue magna. Morbi dolor risus, tincidunt quis tincidunt sed, laoreet sit amet d' +
                   'ui. Nunc tristique diam quis sapien ultrices dignissim. Donec vehicula odio ut est accumsan la' +
                   'oreet. Pellentesque nec sem enim. Vestibulum ante ipsum primis in faucibus orci luctus et ultr' +
                   'ices posuere cubilia Curae; Sed dignissim, neque sit amet tincidunt iaculis, dolor magna laore' +
                   'et libero, non iaculis lectus magna eu sem. Aliqua tincidunt, tellus sed ultrices faucibus, er' +
                   'os dui condimentum orci, condimentum pharetra augue lorem non neque.',
            wrap: true,
            justify: Justify.FILL,
            margin: 10
        });
        
        var secondLabel = new Label({
            text:  'This is a good enough length for any line to have. ' +
                   'Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed blandito dictum massa at varius. ' +
                   'Aenean massa leo, dapibusa et dictum ac, elementum nec elit. Donec turpis massa, cursus ut vol' +
                   'utpat eget, suscipit sit amet massa. Nullam eleifend nulla ut quam fringilla auctor. Suspendis' +
                   'se tempus, mauris a aliquet tristique, dui nunc fringilla erat, in pretium purus risus ut enim' +
                   '. Nullam eget augue magna. Morbi dolor risus, tincidunt quis tincidunt sed, laoreet sit amet d' +
                   'ui. Nunc tristique diam quis sapien ultrices dignissim. Donec vehicula odio ut est accumsan la' +
                   'oreet. Pellentesque nec sem enim. Vestibulum ante ipsum primis in faucibus orci luctus et ultr' +
                   'ices posuere cubilia Curae; Sed dignissim, neque sit amet tincidunt iaculis, dolor magna laore' +
                   'et libero, non iaculis lectus magna eu sem. Aliqua tincidunt, tellus sed ultrices faucibus, er' +
                   'os dui condimentum orci, condimentum pharetra augue lorem non neque.',
            wrap: true,
            justify: Justify.LEFT,
            margin: 10
        });
        
        var paned = new Paned();
        
        var separator = new Separator();
        
        var image = new Image({file: 'chrome.png'});
        
        var spinner = new Spinner({active: true});
        
        var slider = new Slider({adjustment: new Adjustment({lower: 100, upper: 200, value: 150, 'page-size': 20}), digits: 10, 'can-focus': true});
        var otherSlider = new Slider({adjustment: slider.getAdjustment(), inverted: true, 'can-focus': true});
        var anotherSlider = new Slider({adjustment: slider.getAdjustment(), orientation: Orientation.VERTICAL, 'can-focus': true});
        var yetAnotherSlider = new Slider({adjustment: slider.getAdjustment(), inverted: true, orientation: Orientation.VERTICAL, 'can-focus': true});
        
        var scrollBar = new ScrollBar({
            orientation: Orientation.HORIZONTAL,
            inverted: false,
            adjustment: slider.getAdjustment()
        });
        
        var scrollArea = new ScrollArea({'shadow-type': ShadowType.IN, 'h-policy': Policy.ALWAYS, 'v-policy': Policy.NEVER});
        
        var menuBar = new MenuBar();
        var statusBar = new StatusBar();
        var submenu = new Menu();
        var subSubmenu = new Menu();
        
        subSubmenu.add(new MenuItem({label: 'Cut', visible: true}));
        subSubmenu.add(new MenuItem({label: 'Copy', visible: true}));
        subSubmenu.add(new MenuItem({label: 'Paste', visible: true}));
        
        submenu.add(new MenuItem({label: 'Cut', visible: true}));
        submenu.add(new MenuItem({label: 'Copy', visible: true, sensitive: false}));
        submenu.add(new MenuItem({label: 'Submenu', submenu: subSubmenu, visible: true}));
        submenu.add(new SeparatorMenuItem({visible: true}));
        submenu.add(new MenuItem({label: 'Paste', visible: true}));
        
        menuBar.add(new MenuItem({label: 'File'}));
        menuBar.add(new MenuItem({label: 'Edit', submenu: submenu}));
        menuBar.add(new MenuItem({label: 'Help'}));
        
        
        var radioButtonA = new RadioButton({active: true, 'can-focus': true});
        var radioButtonB = new RadioButton({group: radioButtonA.getGroup(), 'can-focus': true});
        var radioButtonC = new RadioButton({active: true, sensitive: false, 'can-focus': true});
        var radioButtonD = new RadioButton({'can-focus': true});
        
        radioButtonB.join(radioButtonC);
        radioButtonA.join(radioButtonD);
        
        radioButtonA.add(new Label({text: 'First option'}));
        radioButtonB.add(new Label({text: 'Second option'}));
        radioButtonC.add(new Label({text: 'Third option'}));
        radioButtonD.add(new Label({text: 'Fourth option'}));
        
        
        
        //frame.add(anotherBox);
        
        
        yetAnotherBox.add(progressBar);
        yetAnotherBox.add(anotherSlider);
        yetAnotherBox.add(otherProgressBar);
        yetAnotherBox.add(yetAnotherSlider);
        
        anotherBox.add(newProgressBar);
        anotherBox.add(slider);
        anotherBox.add(aNewProgressBar);
        anotherBox.add(otherSlider);
        anotherBox.add(yetAnotherBox);
        
        //var frameA = new Frame({'shadow-type': ShadowType.IN});
        //frameA.add(firstLabel);
        
        //var frameB = new Frame({'shadow-type': ShadowType.IN});
        //frameB.add(secondLabel);
        
        var scrollAreaA = new ScrollArea();
        scrollAreaA.add(firstLabel);
        
        var scrollAreaB = new ScrollArea();
        scrollAreaB.add(secondLabel);
        
        paned.add(scrollAreaA, false);
        paned.add(scrollAreaB, true);
        
        
        var foo = new Box({orientation: Orientation.VERTICAL, margin: 10, spacing: 10});
        
        foo.add(paned);
        foo.add(anotherBox);
        foo.add(frame);
        foo.add(button);
        foo.add(spinner);
        foo.add(scrollBar);
        foo.add(radioButtonA);
        foo.add(radioButtonB);
        foo.add(radioButtonC);
        foo.add(radioButtonD);
        foo.add(separator);
        foo.add(image);
        
        scrollArea.add(foo);
        
        box.add(menuBar, false);
        box.add(scrollArea);
        box.add(statusBar, false);
        
        window.add(box);
        
        var hBox = new Box();
        
        hBox.add(new Label({text: "A\nB\nC\nD\nE\nF\nG\nH"}), false);
        hBox.add(new Separator({orientation: Orientation.VERTICAL}), false);
        hBox.add(new Label({text: 'Test'}));
        hBox.add(new Label({text: 'Test'}));
        
        button.add(hBox);
        
        window.showAll();
        
        
        statusBar.push('Some status bar message.');
        
        //*/
        
        /*
        var progress = 10;
        setInterval(function()
        {
            [progressBar, otherProgressBar, newProgressBar, aNewProgressBar].forEach(function(bar)
            {
                var fraction = progress / 100;
                
                bar.setFraction(fraction);
                bar.setText(progress + '%');
            });
            
            progress++;
            if (progress >= 100)
            {
                progress = 10;
                
                spinner.setActive(!spinner.getActive());
            }
        }, 100);
        //*/
        
        
        //*
        
        var anotherWindow = new Window({
            deletable: false,
            maximizable: false,
            resizable: true,
            title: 'Some window',
            opacity: 1.0,
            margin: 0
        });
        
        var bottomBox = new Box({spacing: 5});
        
        bottomBox.setMargin(10);
        
        var a = new Frame({'shadow-type': ShadowType.ETCHED_IN, label: 'Search Mode'});
        var b = new Frame({'shadow-type': ShadowType.ETCHED_IN, label: 'Direction'});
        var c = new Frame({'shadow-type': ShadowType.ETCHED_IN, label: 'Transparancy'});
        
        var searchModeBox = new Box({});
        
        a.setShadowType(ShadowType.ETCHED_OUT);
        
        bottomBox.add(a);
        bottomBox.add(b);
        bottomBox.add(c);
        
        
        
        
        var wholeBox = new Box({orientation: Orientation.VERTICAL, spacing: 0});
        
        wholeBox.setMargin(50);
        
        var buttonBox = new ButtonBox({orientation: Orientation.VERTICAL, spacing: 5, 'layout-style': ButtonBoxStyle.SPREAD});
        
        var isFocus;
        
        buttonBox.add(new Button({label: 'Find Next', 'can-focus': true}));
        buttonBox.add(isFocus = new Button({label: 'Count', 'can-focus': true}));
        buttonBox.add(new Button({label: "Find All in All Openend\nDocuments", 'can-focus': true}));
        buttonBox.add(new Button({label: 'Close', 'can-focus': true}));
        
        var topBox = new Box({margin: 10});
        
        topBox.add(new LineEdit({'sensitive': true, 'text': 'abc', 'can-focus': true}));
        topBox.add(buttonBox);
        
        wholeBox.add(topBox);
        //wholeBox.add(bottomBox);
        
        anotherWindow.add(wholeBox);
        
        
        //anotherWindow.add(new LineEdit({'sensitive': true, 'text': 'abc', 'can-focus': true}));
        
        anotherWindow.showAll();
        
        
        
        //isFocus.setIsFocus(true);
        //radioButtonA.setIsFocus(true);
        
        
        //*/
        
        /*
        
        // Load a Glade interface file.
        var builder = new Builder();
        builder.addFromFile('input.xml');
        
        //*/
        
        /*
        
        var splashScreen    = new Window({margin: 10, decorated: false, modal: true});
        var splashVBox      = new Box({orientation: Orientation.VERTICAL, spacing: 10});
        var splashProgress  = new ProgressBar({'show-text': true});
        var splashLoading   = new Label({text: 'Loading...', justify: Justify.LEFT, 'h-align': 0});
        var splashImage     = new Image({file: 'libreoffice.png'});
        
        splashVBox.add(splashImage, true);
        splashVBox.add(splashLoading, false);
        splashVBox.add(splashProgress, false);
        
        splashScreen.add(splashVBox);
        
        splashImage.connect('load', function()
            {
                splashScreen.showAll();
                
                var percent = 0;
                
                var interval = setInterval(function()
                {
                    percent += Math.random() * 2;
                    if (percent >= 110)
                    {
                        clearInterval(interval);
                        splashScreen.hide();
                    }
                    
                    if (percent < 20)
                        splashLoading.setText('Loading fonts...');
                    else if (percent < 30)
                        splashLoading.setText('Loading images...');
                    else if (percent < 40)
                        splashLoading.setText('Loading settings...');
                    else if (percent < 60)
                        splashLoading.setText('Loading user interface...');
                    else if (percent < 70)
                        splashLoading.setText('Loading menus...');
                    else if (percent < 100)
                        splashLoading.setText('Loading document...');
                    
                    splashProgress.setFraction(percent / 100);
                    splashProgress.setText(Math.round(Math.min(100, percent)) + '%');
                }, 100);
            });
        //*/
    }
});

// Start application.
$(document).ready(
    function()
    {
        // Initialize application.
        Application.initialize();
    }
);
