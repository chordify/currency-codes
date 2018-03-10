# Currency Codes (ISO-4217) [![](https://img.shields.io/hackage/v/currency-codes.svg)](https://hackage.haskell.org/package/currency-codes)

## Overview 

This package mainly provides 2 data types to represent and manipulate currencies according to
the [ISO 4217](https://www.iso.org/iso-4217-currency-codes.html) standard. 

It also provides a handful of instances for common typeclasses (`Show`, `Read`, `Random`, `ToJSON`, `FromJSON`, `Val`, `Data`, `Typeable`, `Generic`, `NFData`). 

## Example

```hs
import           Data.Aeson     (ToJSON(..))
import           Data.Currency  (Currency, Alpha(..))
import           GHC.Generics   (Generic)
import qualified Safe
import qualified Data.Aeson     as Aeson
import qualified Data.Currency  as Currency


myCurrencies :: [Currency]
myCurrencies =
  [ Currency.fromAlpha EUR
  , Currency.fromAlpha USD
  ]

data Transaction = Transaction
  { amount   :: Integer
  , currency :: Currency
  } deriving (Eq, Show, Generic)

instance ToJSON Transaction where
  toJSON = 
    Aeson.genericToJSON Aeson.defaultOptions
```

## License

[MIT © 2017-2018 Chordify](https://gitlab.com/chordify/currency-codes/blob/master/LICENSE)
