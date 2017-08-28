# Currency Codes (ISO-4217) [![](https://img.shields.io/hackage/v/currency-codes.svg)](https://hackage.haskell.org/package/currency-codes)

## Overview 

This package mainly provides 2 data types to represent and manipulate currencies according to
the [ISO 4217](https://www.iso.org/iso-4217-currency-codes.html) standard. 

It also provides a handful of instances for common typeclasses (`Show`, `Read`, `Random`, `ToJSON`,
`FromJSON`, `Val`, `ToSchema`). 

## Example

```haskell
import           Data.Aeson     (ToJSON(..))
import qualified Data.Aeson     as Aeson
import           Data.Currency  (Currency, Alpha)
import qualified Data.Currency  as Currency
import           GHC.Generics   (Generic)
import qualified Safe


myCurrencies :: [Currency]
myCurrencies =
  [Currency.eur, Currency.usdDollar]

fromAlpha :: Alpha -> [Currency] -> Maybe Currency
fromAlpha α =
  Safe.headMay . filter (== α)


data Transaction = Transaction
  { amount   :: Integer
  , currency :: Currency
  } deriving (Eq, Show, Generic)

instance ToJSON Transaction where
  toJSON = 
    Aeson.genericToJSON Aeson.defaultOptions
```

## License

[MIT © 2017 Chordify](https://gitlab.com/chordify/currency-codes/blob/master/LICENSE)
