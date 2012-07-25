module Graphics.UI.WebGUI.Properties.Protocol (
    protoToProp,
    propToProto
  ) where

import Graphics.UI.WebGUI.Tokens
import Graphics.UI.WebGUI.Properties.Props
import Graphics.UI.WebGUI.Properties.Properties
import Graphics.UI.WebGUI.Events.Protocol

-- | Converts a property name and value to the corresponding Prop, or Nothing if the
-- property was not intended to be received.
protoToProp :: (String, Value) -> Maybe Prop
protoToProp t = case t of
                     ("text", StringV v)  -> Just $ TextProp $ Text v
                     ("active", BoolV v)  -> Just $ ActiveProp $ Active v
                     -- TODO: append other valid cases
                     _ -> Nothing

-- | Converts a Prop to its corresponding name and value pair for transmission, or Nothing
-- if the property was not intended to be transmitted.
propToProto :: Prop -> Maybe (String, Value)
propToProto p = case p of
              VisibleProp (Visible v) -> Just ("visible", BoolV v)
              SizeProp (Size (a, b)) -> Just ("size", ListV $ map IntegerV [a, b])
              MarginProp (Margin (a, b, c, d)) -> Just ("margin", ListV $ map IntegerV [a, b, c, d])
              SensitiveProp (Sensitive v) -> Just ("sensitive", BoolV v)
              CanFocusProp (CanFocus v) -> Just ("can-focus", BoolV v)
              TitleProp (Title v) -> Just ("title", StringV v)
              OpacityProp (Opacity v) -> Just ("opacity", FloatV v)
              LabelProp (Label v) -> Just ("label", StringV v)
              TextProp (Text v) -> Just ("text", StringV v)
              EditableProp (Editable v) -> Just ("editable", BoolV v)
              VisibilityProp (Visibility v) -> Just ("visibility", BoolV v)
              MaxLengthProp (MaxLength v) -> Just ("max-length", IntegerV v)
              EventsProp (Events v) -> Just ("events", IntegerV $ sum $ map eventBitmask v)
              HomogeneousProp (Homogeneous v) -> Just ("homogeneous", BoolV v)
              OrientationProp (Orientation v) -> Just ("orientation", StringV v)
              
              ActiveProp (Active v) -> Nothing
              ParentProp (Parent v) -> error "Setting parent through property is deprecated" -- TODO: remove?
