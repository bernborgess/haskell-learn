{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Ui (
    HTMLLucid,
    personAPI,
    server4,
) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Lucid (ToHtml (..), renderBS, table_, td_, th_, tr_)
import Network.HTTP.Media ((//), (/:))
import Servant (
    Accept (..),
    Get,
    JSON,
    MimeRender (..),
    Proxy (..),
    Server,
 )
import Servant.API ((:>))

data HTMLLucid

instance Accept HTMLLucid where
    contentType _ = "text" // "html" /: ("charset", "utf-8")

instance ToHtml a => MimeRender HTMLLucid a where
    mimeRender _ = renderBS . toHtml

type PersonAPI = "persons" :> Get '[JSON, HTMLLucid] [Person]

data Person = Person
    { firstName :: String
    , lastName :: String
    }
    deriving (Generic)

instance ToJSON Person

-- HTML serialization of a single person
instance ToHtml Person where
    toHtml person =
        tr_ $ do
            td_ (toHtml $ firstName person)
            td_ (toHtml $ lastName person)

    toHtmlRaw = toHtml

-- HTML serialization of a list of persons
instance ToHtml [Person] where
    toHtml persons = table_ $ do
        tr_ $ do
            th_ "first name"
            th_ "last name"

        -- this just calls toHtml on each person of the list
        -- and concatenates the resulting pieces of HTML together
        foldMap toHtml persons

    toHtmlRaw = toHtml

people :: [Person]
people =
    [ Person "Isaac" "Newton"
    , Person "Albert" "Einstein"
    ]

personAPI :: Proxy PersonAPI
personAPI = Proxy

server4 :: Server PersonAPI
server4 = return people
