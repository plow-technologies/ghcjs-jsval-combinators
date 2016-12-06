{-# LANGUAGE OverloadedStrings      #-}

module GHCJS.JSVal.Combinators where

import           Control.Monad                (forM)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Maybe

import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8   as L
import           Data.Hashable
import qualified Data.HashMap.Lazy            as HML
import           Data.Maybe                   (catMaybes)
import qualified Data.JSString                as JS (unpack, length, take, drop)
import           Data.Monoid                  ((<>))


import           GHCJS.Marshal
import           GHCJS.Types

import qualified JavaScript.Object            as JSO

import           Unsafe.Coerce

foreign import javascript unsafe "(function () { var tst = typeof $1; if (tst == \"string\"){ var rslt = JSON.parse($1); return rslt; } else if (tst == \"object\"){ return $1;}} ())"
  js_convertObject :: JSVal -> IO JSO.Object

createObject :: [IO (JSString, JSVal)] -> IO JSVal
createObject props = do
  obj <- JSO.create
  mapM_ (\(name, prop) -> JSO.setProp name prop obj) =<< sequence props
  return $ unsafeCoerce obj

getPropMaybe :: FromJSVal a => JSString -> JSVal -> IO (Maybe a)
getPropMaybe name jsv = do
  obj <- js_convertObject jsv
  mProp <- JSO.getProp name obj
  if isNull mProp
    then do
      putStrLn $ "IsNull: " ++ JS.unpack name
      return Nothing
    else fromJSVal mProp

getPropMaybe' :: FromJSVal a => JSString -> JSVal -> IO (Maybe (Maybe a))
getPropMaybe' name jsv = do
  let obj = unsafeCoerce jsv
  mProp <- JSO.getProp name obj
  if isNull mProp
    then return . Just $ Nothing
    else Just <$> fromJSVal mProp

-- useful for creating sum types in FromJSVal
-- need to be able to run HML.member "key" on the resulting value
-- the second value is not important

foreign import javascript unsafe  "Object.keys($1)"
  js_listObjectKeys :: JSVal -> IO JSVal
  
jsValToHashMap :: JSVal -> IO (HML.HashMap String ())
jsValToHashMap jsv = do
  jsValProps <- js_listObjectKeys jsv
  mProps <- fromJSVal jsValProps :: IO (Maybe [String])
  case mProps of
    Nothing    -> return $ HML.fromList []
    Just props -> return $ HML.fromList $ (,) <$> props <*> [()]

-- this is old code, leave for reference for now
-- for some reason JSO.listProps seems to be broken
{-
jsValToHashMap :: JSVal -> IO (HML.HashMap String ())
jsValToHashMap jsv = do
  obj <- js_convertObject jsv
  props <- JSO.listProps obj
  mVals <- forM props $ \prop -> do
    mmVal <- JSO.getProp prop obj
    if isNull mmVal
      then return Nothing
      else return $ Just (JS.unpack prop, ())
  return $ HML.fromList $ catMaybes mVals
-}
(.->) :: FromJSVal a => JSVal -> JSString -> MaybeT IO a
obj .-> name = MaybeT $ getPropMaybe name $ unsafeCoerce obj


(.->?) :: FromJSVal a => JSVal -> JSString -> MaybeT IO (Maybe a)
obj .->? name = MaybeT $ getPropMaybe' name $ unsafeCoerce obj


(.~>) :: FromJSON a => JSVal -> JSString -> MaybeT IO a
obj .~> name = MaybeT $ do
  strVal <- liftIO $ JSO.getProp name $ unsafeCoerce obj
  if isNull strVal
    then do
      return Nothing
    else do
      str <- fromJSVal strVal
      let rslt = parseJSVal =<<  maybe (Left "Error reading value fromJSVal") (Right) (L.pack <$> str)
      case rslt of
        Left err -> do
          putStrLn err
          return Nothing
        Right x -> return x

newtype JSONData a = JSONData { unJSONData :: a } deriving (Eq, Show)

instance (FromJSON a) => FromJSON (JSONData a) where
  parseJSON (Data.Aeson.Object v) = JSONData <$> v .: "data"
  parseJSON _ = fail "Error: Expecting object, received other"


parseJSVal :: FromJSON a => L.ByteString -> Either String a
parseJSVal str = case unJSONData <$> rslt of
                    (Left err) -> Left err
                    (Right unJSON) -> Right unJSON
  where rslt = eitherDecode asList
        asList = "{\"data\":\"" <> str <> "\"}"

(.~>?) :: FromJSON a => JSVal -> JSString -> MaybeT IO (Maybe a)
obj .~>? name = MaybeT $ do
  strVal <- liftIO $ JSO.getProp name $ unsafeCoerce obj
  if isNull strVal
    then do
      return $ Just Nothing
    else do
      str <- fromJSVal strVal
      let rslt = parseJSVal =<<  maybe (Left "Error reading value fromJSVal") (Right) (L.pack <$> str)
      case rslt of
        Left err -> do
          putStrLn err
          return $ Just Nothing
        Right x -> return $ Just x


(.==>) :: ToJSON a => JSString -> a -> IO (JSString, JSVal)
name .==> val = (\x -> (name,x)) <$> toJSVal_aeson val

(.=>) :: (ToJSVal a) => JSString -> a -> IO (JSString, JSVal)
name .=> prop = (\x -> (name,x)) <$> toJSVal prop
