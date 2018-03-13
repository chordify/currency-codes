-- | A Currency representation as specified in the ISO-4217 standard

module Data.Currency
  -- * Types
  ( Currency(..)
  , Alpha(..)
  , currencies

  -- * Create utilities
  , fromAlpha
  ) where

import           Prelude         hiding (Ordering (..))

import           Control.DeepSeq (NFData)
import           Control.Monad   ((>=>))
import           Data.Aeson      (FromJSON (..), ToJSON (..))
import           Data.Bson       (Val (..), (=:))
import           Data.Data       (Data, dataTypeConstrs, dataTypeOf, toConstr)
import           Data.List       (elemIndex)
import           Data.Text       (Text)
import           Data.Typeable   (Typeable)
import           GHC.Generics    (Generic)
import           System.Random   (Random (..))

import qualified Data.Aeson      as Aeson
import qualified Data.Bson       as Bson
import qualified Safe


-- | Actual representation of a currency
data Currency = Currency
  { alpha   :: !Alpha -- ^ Alpha Code
  , numeric :: !Int   -- ^ Numeric code
  , minor   :: !Int   -- ^ Number of decimal units
  , name    :: !Text  -- ^ English name
  } deriving (Show, Eq, Read, Generic, Data, Typeable)


instance NFData Currency


instance FromJSON Currency where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions


instance ToJSON Currency where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions


instance Val Currency where
  val Currency{..} = Bson.Doc
    [ "alpha"   =: alpha
    , "numeric" =: numeric
    , "minor"   =: minor
    , "name"    =: name
    ]

  cast' = cast' >=> \doc -> Currency
    <$> Bson.lookup "alpha"   doc
    <*> Bson.lookup "numeric" doc
    <*> Bson.lookup "minor"   doc
    <*> Bson.lookup "name"    doc


instance Random Currency where
  random g =
    let
      (i, g') =
        randomR (0, length currencies - 1) g
    in
      (currencies !! i, g')

  randomR (a, b) g =
    let
      currencies' =
        dropWhile (/= a) $ takeWhile (/= b) currencies

      (i, g') =
        randomR (0, length currencies' - 1) g
    in
      (currencies' !! i, g')


-- | A type which represents ISO 4217 alphabetic codes as an enum
data Alpha
  = AED  -- ^ UAE Dirham
  | AFN  -- ^ Afghani
  | ALL  -- ^ Lek
  | AMD  -- ^ Armenian Dram
  | ANG  -- ^ Netherlands Antillean Guilder
  | AOA  -- ^ Kwanza
  | ARS  -- ^ Argentine Peso
  | AUD  -- ^ Australian Dollar
  | AWG  -- ^ Aruban Florin
  | AZN  -- ^ Azerbaijan Manat
  | BAM  -- ^ Convertible Mark
  | BBD  -- ^ Barbados Dollar
  | BDT  -- ^ Taka
  | BGN  -- ^ Bulgarian Lev
  | BHD  -- ^ Bahraini Dinar
  | BIF  -- ^ Burundi Franc
  | BMD  -- ^ Bermudian Dollar
  | BND  -- ^ Brunei Dollar
  | BOB  -- ^ Boliviano
  | BOV  -- ^ Mvdol
  | BRL  -- ^ Brazilian Real
  | BSD  -- ^ Bahamian Dollar
  | BTN  -- ^ Ngultrum
  | BWP  -- ^ Pula
  | BYN  -- ^ Belarusian Ruble
  | BZD  -- ^ Belize Dollar
  | CAD  -- ^ Canadian Dollar
  | CDF  -- ^ Congolese Franc
  | CHE  -- ^ WIR Euro
  | CHF  -- ^ Swiss Franc
  | CHW  -- ^ WIR Franc
  | CLF  -- ^ Unidad de Fomento
  | CLP  -- ^ Chilean Peso
  | CNY  -- ^ Yuan Renminbi
  | COP  -- ^ Colombian Peso
  | COU  -- ^ Unidad de Valor Real
  | CRC  -- ^ Costa Rican Colon
  | CUC  -- ^ Peso Convertible
  | CUP  -- ^ Cuban Peso
  | CVE  -- ^ Cabo Verde Escudo
  | CZK  -- ^ Czech Koruna
  | DJF  -- ^ Djibouti Franc
  | DKK  -- ^ Danish Krone
  | DOP  -- ^ Dominican Peso
  | DZD  -- ^ Algerian Dinar
  | EGP  -- ^ Egyptian Pound
  | ERN  -- ^ Nakfa
  | ETB  -- ^ Ethiopian Birr
  | EUR  -- ^ Euro
  | FJD  -- ^ Fiji Dollar
  | FKP  -- ^ Falkland Islands Pound
  | GBP  -- ^ Pound Sterling
  | GEL  -- ^ Lari
  | GHS  -- ^ Ghana Cedi
  | GIP  -- ^ Gibraltar Pound
  | GMD  -- ^ Dalasi
  | GNF  -- ^ Guinean Franc
  | GTQ  -- ^ Quetzal
  | GYD  -- ^ Guyana Dollar
  | HKD  -- ^ Hong Kong Dollar
  | HNL  -- ^ Lempira
  | HRK  -- ^ Kuna
  | HTG  -- ^ Gourde
  | HUF  -- ^ Forint
  | IDR  -- ^ Rupiah
  | ILS  -- ^ New Israeli Sheqel
  | INR  -- ^ Indian Rupee
  | IQD  -- ^ Iraqi Dinar
  | IRR  -- ^ Iranian Rial
  | ISK  -- ^ Iceland Krona
  | JMD  -- ^ Jamaican Dollar
  | JOD  -- ^ Jordanian Dinar
  | JPY  -- ^ Yen
  | KES  -- ^ Kenyan Shilling
  | KGS  -- ^ Som
  | KHR  -- ^ Riel
  | KMF  -- ^ Comorian Franc
  | KPW  -- ^ North Korean Won
  | KRW  -- ^ Won
  | KWD  -- ^ Kuwaiti Dinar
  | KYD  -- ^ Cayman Islands Dollar
  | KZT  -- ^ Tenge
  | LAK  -- ^ Lao Kip
  | LBP  -- ^ Lebanese Pound
  | LKR  -- ^ Sri Lanka Rupee
  | LRD  -- ^ Liberian Dollar
  | LSL  -- ^ Loti
  | LYD  -- ^ Libyan Dinar
  | MAD  -- ^ Moroccan Dirham
  | MDL  -- ^ Moldovan Leu
  | MGA  -- ^ Malagasy Ariary
  | MKD  -- ^ Denar
  | MMK  -- ^ Kyat
  | MNT  -- ^ Tugrik
  | MOP  -- ^ Pataca
  | MRO  -- ^ Ouguiya
  | MUR  -- ^ Mauritius Rupee
  | MVR  -- ^ Rufiyaa
  | MWK  -- ^ Malawi Kwacha
  | MXN  -- ^ Mexican Peso
  | MXV  -- ^ Mexican Unidad de Inversion (UDI)
  | MYR  -- ^ Malaysian Ringgit
  | MZN  -- ^ Mozambique Metical
  | NAD  -- ^ Namibia Dollar
  | NGN  -- ^ Naira
  | NIO  -- ^ Cordoba Oro
  | NOK  -- ^ Norwegian Krone
  | NPR  -- ^ Nepalese Rupee
  | NZD  -- ^ New Zealand Dollar
  | OMR  -- ^ Rial Omani
  | PAB  -- ^ Balboa
  | PEN  -- ^ Sol
  | PGK  -- ^ Kina
  | PHP  -- ^ Philippine Peso
  | PKR  -- ^ Pakistan Rupee
  | PLN  -- ^ Zloty
  | PYG  -- ^ Guarani
  | QAR  -- ^ Qatari Rial
  | RON  -- ^ Romanian Leu
  | RSD  -- ^ Serbian Dinar
  | RUB  -- ^ Russian Ruble
  | RWF  -- ^ Rwanda Franc
  | SAR  -- ^ Saudi Riyal
  | SBD  -- ^ Solomon Islands Dollar
  | SCR  -- ^ Seychelles Rupee
  | SDG  -- ^ Sudanese Pound
  | SEK  -- ^ Swedish Krona
  | SGD  -- ^ Singapore Dollar
  | SHP  -- ^ Saint Helena Pound
  | SLL  -- ^ Leone
  | SOS  -- ^ Somali Shilling
  | SRD  -- ^ Surinam Dollar
  | SSP  -- ^ South Sudanese Pound
  | STD  -- ^ Dobra
  | SVC  -- ^ El Salvador Colon
  | SYP  -- ^ Syrian Pound
  | SZL  -- ^ Lilangeni
  | THB  -- ^ Baht
  | TJS  -- ^ Somoni
  | TMT  -- ^ Turkmenistan New Manat
  | TND  -- ^ Tunisian Dinar
  | TOP  -- ^ Pa’anga
  | TRY  -- ^ Turkish Lira
  | TTD  -- ^ Trinidad and Tobago Dollar
  | TWD  -- ^ New Taiwan Dollar
  | TZS  -- ^ Tanzanian Shilling
  | UAH  -- ^ Hryvnia
  | UGX  -- ^ Uganda Shilling
  | USD  -- ^ US Dollar
  | USN  -- ^ US Dollar (Next day)
  | UYI  -- ^ Uruguay Peso en Unidades Indexadas (URUIURUI)
  | UYU  -- ^ Peso Uruguayo
  | UZS  -- ^ Uzbekistan Sum
  | VEF  -- ^ Bolívar
  | VND  -- ^ Dong
  | VUV  -- ^ Vatu
  | WST  -- ^ Tala
  | XAF  -- ^ CFA Franc BEAC
  | XAG  -- ^ Silver
  | XAU  -- ^ Gold
  | XBA  -- ^ Bond Markets Unit European Composite Unit (EURCO)
  | XBB  -- ^ Bond Markets Unit European Monetary Unit (E.M.U.-6)
  | XBC  -- ^ Bond Markets Unit European Unit of Account 9 (E.U.A.-9)
  | XBD  -- ^ Bond Markets Unit European Unit of Account 17 (E.U.A.-17)
  | XCD  -- ^ East Caribbean Dollar
  | XDR  -- ^ SDR (Special Drawing Right)
  | XOF  -- ^ CFA Franc BCEAO
  | XPD  -- ^ Palladium
  | XPF  -- ^ CFP Franc
  | XPT  -- ^ Platinum
  | XSU  -- ^ Sucre
  | XTS  -- ^ Codes specifically reserved for testing purposes
  | XUA  -- ^ ADB Unit of Account
  | XXX  -- ^ The codes assigned for transactions where no currency is involved
  | YER  -- ^ Yemeni Rial
  | ZAR  -- ^ Rand
  | ZMW  -- ^ Zambian Kwacha
  | ZWL  -- ^ Zimbabwe Dollar
  deriving (Eq, Ord, Enum, Bounded, Generic, Data, Typeable)


instance NFData Alpha


instance Show Alpha where
  show =
    show . toConstr


instance Read Alpha where
  readsPrec _ str =
    let
      (α, rest) = splitAt 3 (dropWhile (== ' ') str)
    in
      case elemIndex α alphas of
        Just i ->
          [(toEnum i, rest)]

        Nothing ->
          []


instance FromJSON Alpha where
  parseJSON =
    Aeson.genericParseJSON Aeson.defaultOptions


instance ToJSON Alpha where
  toJSON =
    Aeson.genericToJSON Aeson.defaultOptions


instance Val Alpha where
  val =
    val . show

  cast' =
    cast' >=> Safe.readMay


instance Random Alpha where
  random g =
    let
      (r, g') =
        randomR (fromEnum (minBound :: Alpha), fromEnum(maxBound :: Alpha)) g
    in
      (toEnum r, g')

  randomR (l, h) g =
    let
      (r, g') =
        randomR (fromEnum l, fromEnum h) g
    in
      (toEnum r, g')


-- Create utilities

-- | Obtain a Currency from its Alpha code
fromAlpha :: Alpha -> Currency
fromAlpha α =
  case α of
    AED -> Currency AED 784 2 "UAE Dirham"
    AFN -> Currency AFN 971 2 "Afghani"
    ALL -> Currency ALL 008 2 "Lek"
    AMD -> Currency AMD 051 2 "Armenian Dram"
    ANG -> Currency ANG 532 2 "Netherlands Antillean Guilder"
    AOA -> Currency AOA 973 2 "Kwanza"
    ARS -> Currency ARS 032 2 "Argentine Peso"
    AUD -> Currency AUD 036 2 "Australian Dollar"
    AWG -> Currency AWG 533 2 "Aruban Florin"
    AZN -> Currency AZN 944 2 "Azerbaijan Manat"
    BAM -> Currency BAM 977 2 "Convertible Mark"
    BBD -> Currency BBD 052 2 "Barbados Dollar"
    BDT -> Currency BDT 050 2 "Taka"
    BGN -> Currency BGN 975 2 "Bulgarian Lev"
    BHD -> Currency BHD 048 3 "Bahraini Dinar"
    BIF -> Currency BIF 108 0 "Burundi Franc"
    BMD -> Currency BMD 060 2 "Bermudian Dollar"
    BND -> Currency BND 096 2 "Brunei Dollar"
    BOB -> Currency BOB 068 2 "Boliviano"
    BOV -> Currency BOV 984 2 "Mvdol"
    BRL -> Currency BRL 986 2 "Brazilian Real"
    BSD -> Currency BSD 044 2 "Bahamian Dollar"
    BTN -> Currency BTN 064 2 "Ngultrum"
    BWP -> Currency BWP 072 2 "Pula"
    BYN -> Currency BYN 933 2 "Belarusian Ruble"
    BZD -> Currency BZD 084 2 "Belize Dollar"
    CAD -> Currency CAD 124 2 "Canadian Dollar"
    CDF -> Currency CDF 976 2 "Congolese Franc"
    CHE -> Currency CHE 947 2 "WIR Euro"
    CHF -> Currency CHF 756 2 "Swiss Franc"
    CHW -> Currency CHW 948 2 "WIR Franc"
    CLF -> Currency CLF 990 4 "Unidad de Fomento"
    CLP -> Currency CLP 152 0 "Chilean Peso"
    CNY -> Currency CNY 156 2 "Yuan Renminbi"
    COP -> Currency COP 170 2 "Colombian Peso"
    COU -> Currency COU 970 2 "Unidad de Valor Real"
    CRC -> Currency CRC 188 2 "Costa Rican Colon"
    CUC -> Currency CUC 931 2 "Peso Convertible"
    CUP -> Currency CUP 192 2 "Cuban Peso"
    CVE -> Currency CVE 132 2 "Cabo Verde Escudo"
    CZK -> Currency CZK 203 2 "Czech Koruna"
    DJF -> Currency DJF 262 0 "Djibouti Franc"
    DKK -> Currency DKK 208 2 "Danish Krone"
    DOP -> Currency DOP 214 2 "Dominican Peso"
    DZD -> Currency DZD 012 2 "Algerian Dinar"
    EGP -> Currency EGP 818 2 "Egyptian Pound"
    ERN -> Currency ERN 232 2 "Nakfa"
    ETB -> Currency ETB 230 2 "Ethiopian Birr"
    EUR -> Currency EUR 978 2 "Euro"
    FJD -> Currency FJD 242 2 "Fiji Dollar"
    FKP -> Currency FKP 238 2 "Falkland Islands Pound"
    GBP -> Currency GBP 826 2 "Pound Sterling"
    GEL -> Currency GEL 981 2 "Lari"
    GHS -> Currency GHS 936 2 "Ghana Cedi"
    GIP -> Currency GIP 292 2 "Gibraltar Pound"
    GMD -> Currency GMD 270 2 "Dalasi"
    GNF -> Currency GNF 324 0 "Guinean Franc"
    GTQ -> Currency GTQ 320 2 "Quetzal"
    GYD -> Currency GYD 328 2 "Guyana Dollar"
    HKD -> Currency HKD 344 2 "Hong Kong Dollar"
    HNL -> Currency HNL 340 2 "Lempira"
    HRK -> Currency HRK 191 2 "Kuna"
    HTG -> Currency HTG 332 2 "Gourde"
    HUF -> Currency HUF 348 2 "Forint"
    IDR -> Currency IDR 360 2 "Rupiah"
    ILS -> Currency ILS 376 2 "New Israeli Sheqel"
    INR -> Currency INR 356 2 "Indian Rupee"
    IQD -> Currency IQD 368 3 "Iraqi Dinar"
    IRR -> Currency IRR 364 2 "Iranian Rial"
    ISK -> Currency ISK 352 0 "Iceland Krona"
    JMD -> Currency JMD 388 2 "Jamaican Dollar"
    JOD -> Currency JOD 400 3 "Jordanian Dinar"
    JPY -> Currency JPY 392 0 "Yen"
    KES -> Currency KES 404 2 "Kenyan Shilling"
    KGS -> Currency KGS 417 2 "Som"
    KHR -> Currency KHR 116 2 "Riel"
    KMF -> Currency KMF 174 0 "Comorian Franc"
    KPW -> Currency KPW 408 2 "North Korean Won"
    KRW -> Currency KRW 410 0 "Won"
    KWD -> Currency KWD 414 3 "Kuwaiti Dinar"
    KYD -> Currency KYD 136 2 "Cayman Islands Dollar"
    KZT -> Currency KZT 398 2 "Tenge"
    LAK -> Currency LAK 418 2 "Lao Kip"
    LBP -> Currency LBP 422 2 "Lebanese Pound"
    LKR -> Currency LKR 144 2 "Sri Lanka Rupee"
    LRD -> Currency LRD 430 2 "Liberian Dollar"
    LSL -> Currency LSL 426 2 "Loti"
    LYD -> Currency LYD 434 3 "Libyan Dinar"
    MAD -> Currency MAD 504 2 "Moroccan Dirham"
    MDL -> Currency MDL 498 2 "Moldovan Leu"
    MGA -> Currency MGA 969 2 "Malagasy Ariary"
    MKD -> Currency MKD 807 2 "Denar"
    MMK -> Currency MMK 104 2 "Kyat"
    MNT -> Currency MNT 496 2 "Tugrik"
    MOP -> Currency MOP 446 2 "Pataca"
    MRO -> Currency MRO 478 2 "Ouguiya"
    MUR -> Currency MUR 480 2 "Mauritius Rupee"
    MVR -> Currency MVR 462 2 "Rufiyaa"
    MWK -> Currency MWK 454 2 "Malawi Kwacha"
    MXN -> Currency MXN 484 2 "Mexican Peso"
    MXV -> Currency MXV 979 2 "Mexican Unidad de Inversion (UDI)"
    MYR -> Currency MYR 458 2 "Malaysian Ringgit"
    MZN -> Currency MZN 943 2 "Mozambique Metical"
    NAD -> Currency NAD 516 2 "Namibia Dollar"
    NGN -> Currency NGN 566 2 "Naira"
    NIO -> Currency NIO 558 2 "Cordoba Oro"
    NOK -> Currency NOK 578 2 "Norwegian Krone"
    NPR -> Currency NPR 524 2 "Nepalese Rupee"
    NZD -> Currency NZD 554 2 "New Zealand Dollar"
    OMR -> Currency OMR 512 3 "Rial Omani"
    PAB -> Currency PAB 590 2 "Balboa"
    PEN -> Currency PEN 604 2 "Sol"
    PGK -> Currency PGK 598 2 "Kina"
    PHP -> Currency PHP 608 2 "Philippine Peso"
    PKR -> Currency PKR 586 2 "Pakistan Rupee"
    PLN -> Currency PLN 985 2 "Zloty"
    PYG -> Currency PYG 600 0 "Guarani"
    QAR -> Currency QAR 634 2 "Qatari Rial"
    RON -> Currency RON 946 2 "Romanian Leu"
    RSD -> Currency RSD 941 2 "Serbian Dinar"
    RUB -> Currency RUB 643 2 "Russian Ruble"
    RWF -> Currency RWF 646 0 "Rwanda Franc"
    SAR -> Currency SAR 682 2 "Saudi Riyal"
    SBD -> Currency SBD 090 2 "Solomon Islands Dollar"
    SCR -> Currency SCR 690 2 "Seychelles Rupee"
    SDG -> Currency SDG 938 2 "Sudanese Pound"
    SEK -> Currency SEK 752 2 "Swedish Krona"
    SGD -> Currency SGD 702 2 "Singapore Dollar"
    SHP -> Currency SHP 654 2 "Saint-Helena Pound"
    SLL -> Currency SLL 694 2 "Leone"
    SOS -> Currency SOS 706 2 "Somali Shilling"
    SRD -> Currency SRD 968 2 "Surinam Dollar"
    SSP -> Currency SSP 728 2 "South Sudanese Pound"
    STD -> Currency STD 678 2 "Dobra"
    SVC -> Currency SVC 222 2 "El Salvador Colon"
    SYP -> Currency SYP 760 2 "Syrian Pound"
    SZL -> Currency SZL 748 2 "Lilangeni"
    THB -> Currency THB 764 2 "Baht"
    TJS -> Currency TJS 972 2 "Somoni"
    TMT -> Currency TMT 934 2 "Turkmenistan New Manat"
    TND -> Currency TND 788 3 "Tunisian Dinar"
    TOP -> Currency TOP 776 2 "Pa’anga"
    TRY -> Currency TRY 949 2 "Turkish Lira"
    TTD -> Currency TTD 780 2 "Trinidad and Tobago Dollar"
    TWD -> Currency TWD 901 2 "New Taiwan Dollar"
    TZS -> Currency TZS 834 2 "Tanzanian Shilling"
    UAH -> Currency UAH 980 2 "Hryvnia"
    UGX -> Currency UGX 800 0 "Uganda"
    USD -> Currency USD 840 2 "US Dollar"
    USN -> Currency USN 997 2 "US Dollar (Next day)"
    UYI -> Currency UYI 940 0 "Uruguay Peso en Unidades Indexadas (URUIURUI)"
    UYU -> Currency UYU 858 2 "Peso Uruguayo"
    UZS -> Currency UZS 860 2 "Uzbekistan"
    VEF -> Currency VEF 937 2 "Bolívar"
    VND -> Currency VND 704 0 "Dong"
    VUV -> Currency VUV 548 0 "Vatu"
    WST -> Currency WST 882 2 "Tala"
    XAF -> Currency XAF 950 0 "CFA Franc BEAC"
    XAG -> Currency XAG 961 0 "Silver"
    XAU -> Currency XAU 959 0 "Gold"
    XBA -> Currency XBA 955 0 "Bond Markets Unit European Composite Unit (EURCO)"
    XBB -> Currency XBB 956 0 "Bond Markets Unit European Monetary Unit (E.M.U.-6)"
    XBC -> Currency XBC 957 0 "Bond Markets Unit European Unit of Account 9 (E.U.A.-9)"
    XBD -> Currency XBD 958 0 "Bond Markets Unit European Unit of Account 17 (E.U.A.-17)"
    XCD -> Currency XCD 951 2 "East Caribbean Dollar"
    XDR -> Currency XDR 960 0 "SDR (Special Drawing Right)"
    XOF -> Currency XOF 952 0 "CFA Franc BCEAO"
    XPD -> Currency XPD 964 0 "Palladium"
    XPF -> Currency XPF 953 0 "CFP Franc"
    XPT -> Currency XPT 962 0 "Platinum"
    XSU -> Currency XSU 994 0 "Sucre"
    XTS -> Currency XTS 963 0 "Codes specifically reserved for testing purposes"
    XUA -> Currency XUA 965 0 "ADB Unit of Account"
    XXX -> Currency XXX 999 0 "The codes assigned for transactions where no currency is involved"
    YER -> Currency YER 886 2 "Yemeni Rial"
    ZAR -> Currency ZAR 710 2 "Rand"
    ZMW -> Currency ZMW 967 2 "Zambian Kwacha"
    ZWL -> Currency ZWL 932 2 "Zimbabwe Dollar"


-- | List of all currencies in the standard
currencies :: [Currency]
currencies =
  fmap fromAlpha [minBound..maxBound]


-- | List of all alpha constructors as strings
alphas :: [String]
alphas =
  fmap show $ dataTypeConstrs $ dataTypeOf (minBound :: Alpha)
