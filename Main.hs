module Main where

import ExampleGUI

main = startGUI logic

-- Main application logic

logic :: Screen -> IO ()
logic screen = do mainWindow <- newMainWindow screen
                  set mainWindow [Title := "A GUI application"]

                  window <- newWindow screen
                  box <- newBox screen
                  button <- newButton screen
                  entry <- newLineEdit screen
                  entry2 <- newLineEdit screen
                
                  add window box
                  add box button
                  add box entry
                  add box entry2
                
                  -- Set some properties
                  set window [Title := "Test Window"]
                  set button [Label := "Click me",
                              CanFocus := True,
                              Visible := True]
                  set entry [Text := "Type here",
                             CanFocus := True,
                             Visible := True]
                  set entry2 [CanFocus := True,
                              Visible := True]
                  set box [Visible := True,
                           Orientation := "vertical"]
                 
                  -- Finally show window, to correctly center it
                  set window [Visible := True]
                
                  -- Add a button to the main window
                  button2 <- newButton screen
                  set button2 [Visible := True,
                               Label := "Main button label",
                               CanFocus := True]

                  set mainWindow [Visible := True]
                
                  add mainWindow button2
                
                  -- Capture button releases on button
                  enableEvents button [ButtonReleaseEvent, ButtonPressEvent, FocusEvent, BlurEvent]
                  on button (Change Active) $ const (do Text a <- get entry Text
                                                        set button [Label := ("You typed: " ++ a)]
                                                        set entry [Text := ""])
                  -- Monitor for changes on entry text
                  on entry (Change Text) $ const (do Text a <- get entry Text
                                                     case a of
                                                       "quit" -> quit screen
                                                       _      -> set entry2 [Text := a])
                                                     
