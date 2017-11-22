module Custom.Feed where

import Hakyll


myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
  { feedTitle = "Shou's origin"
  , feedDescription = "Source of everything"
  , feedAuthorName = "Shou Ya"
  , feedAuthorEmail = "log AT lain.li"
  , feedRoot        = "https://log.lain.li"
  }
