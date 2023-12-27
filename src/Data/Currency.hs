-- | A Currency representation as specified in the ISO-4217 standard

module Data.Currency
  (
  -- * Types
  Currency(..)
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
  , minor   :: !Int   -- ^ Number of decimal units. This may not be accurate for inactive currencies.
  , name    :: !Text  -- ^ English name
  , active  :: !Bool  -- ^ True if the currency is still in circulation
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
    , "active"  =: active
    ]

  cast' = cast' >=> \doc -> Currency
    <$> Bson.lookup "alpha"   doc
    <*> Bson.lookup "numeric" doc
    <*> Bson.lookup "minor"   doc
    <*> Bson.lookup "name"    doc
    <*> Bson.lookup "active"  doc


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
  | MRU  -- ^ Ouguiya
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
  | SLL  -- ^ Old Leone
  | SLE  -- ^ Leone
  | SOS  -- ^ Somali Shilling
  | SRD  -- ^ Surinam Dollar
  | SSP  -- ^ South Sudanese Pound
  | STN  -- ^ Dobra
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
  | VES  -- ^ Bolívar
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
  
  | ADP -- ^ Andorran peseta
  | AFA -- ^ Afghan afghani
  | ALK -- ^ Old Albanian lek
  | AOK -- ^ Angolan kwanza
  | AON -- ^ Angolan novo kwanza
  | AOR -- ^ Angolan kwanza reajustado
  | ARA -- ^ Argentine austral
  | ARP -- ^ Argentine peso argentino
  | ARY -- ^ Argentine peso ley
  | ATS -- ^ Austrian schilling
  | AYM -- ^ Azerbaijani manat
  | AZM -- ^ Azerbaijani manat
  | BAD -- ^ Bosnia and Herzegovina dinar
  | BEC -- ^ Belgian convertible franc (funds code)
  | BEF -- ^ Belgian franc
  | BEL -- ^ Belgian financial franc (funds code)
  | BGJ -- ^ Bulgarian lev (first)
  | BGK -- ^ Bulgarian lev (second)
  | BGL -- ^ Bulgarian lev (third)
  | BOP -- ^ Bolivian peso
  | BRB -- ^ Brazilian cruzeiro
  | BRC -- ^ Brazilian cruzado
  | BRE -- ^ Brazilian cruzeiro
  | BRN -- ^ Brazilian cruzado novo
  | BRR -- ^ Brazilian cruzeiro real
  | BUK -- ^ Burmese kyat
  | BYB -- ^ Belarusian ruble
  | BYR -- ^ Belarusian ruble
  | CHC -- ^ WIR franc (for electronic currency)
  | CSD -- ^ Serbian dinar
  | CSJ -- ^ Czechoslovak koruna (second)
  | CSK -- ^ Czechoslovak koruna
  | CUC -- ^ Cuban convertible peso
  | CYP -- ^ Cypriot pound
  | DDM -- ^ East German mark
  | DEM -- ^ German mark
  | ECS -- ^ Ecuadorian sucre
  | ECV -- ^ Ecuador Unidad de Valor Constante (funds code)
  | EEK -- ^ Estonian kroon
  | ESA -- ^ Spanish peseta (account A)
  | ESB -- ^ Spanish peseta (account B)
  | ESP -- ^ Spanish peseta
  | FIM -- ^ Finnish markka
  | FRF -- ^ French franc
  | GEK -- ^ Georgian kuponi
  | GHC -- ^ Ghanaian cedi
  | GHP -- ^ Ghanaian cedi
  | GNE -- ^ Guinean syli
  | GNS -- ^ Guinean syli
  | GQE -- ^ Equatorial Guinean ekwele
  | GRD -- ^ Greek drachma
  | GWE -- ^ Guinean escudo
  | GWP -- ^ Guinea-Bissau peso
  | HRD -- ^ Croatian dinar
  | HRK -- ^ Croatian kuna
  | IEP -- ^ Irish pound
  | ILP -- ^ Israeli pound
  | ILR -- ^ Israeli shekel
  | ISJ -- ^ Icelandic króna
  | ITL -- ^ Italian lira
  | LAJ -- ^ Lao kip
  | LSM -- ^ Lesotho loti
  | LTL -- ^ Lithuanian litas
  | LTT -- ^ Lithuanian talonas
  | LUC -- ^ Luxembourg convertible franc (funds code)
  | LUF -- ^ Luxembourg franc
  | LUL -- ^ Luxembourg financial franc (funds code)
  | LVL -- ^ Latvian lats
  | LVR -- ^ Latvian rublis
  | MGF -- ^ Malagasy franc
  | MLF -- ^ Malian franc
  | MRO -- ^ Mauritanian ouguiya
  | MTL -- ^ Maltese lira
  | MTP -- ^ Maltese pound
  | MVQ -- ^ Maldivian rupee
  | MXP -- ^ Mexican peso
  | MZE -- ^ Mozambican escudo
  | MZM -- ^ Mozambican metical
  | NIC -- ^ Nicaraguan córdoba
  | NLG -- ^ Dutch guilder
  | PEH -- ^ Peruvian old sol
  | PEI -- ^ Peruvian inti
  | PES -- ^ Peruvian sol
  | PLZ -- ^ Polish zloty
  | PTE -- ^ Portuguese escudo
  | RHD -- ^ Rhodesian dollar
  | ROK -- ^ Romanian leu (second)
  | ROL -- ^ Romanian leu (third)
  | RUR -- ^ Russian ruble
  | SDD -- ^ Sudanese dinar
  | SDP -- ^ Sudanese old pound
  | SIT -- ^ Slovenian tolar
  | SKK -- ^ Slovak koruna
  | SRG -- ^ Surinamese guilder
  | STD -- ^ São Tomé and Príncipe dobra (old)
  | SUR -- ^ Soviet Union ruble
  | TJR -- ^ Tajikistani ruble
  | TMM -- ^ Turkmenistani manat
  | TPE -- ^ Portuguese Timorese escudo
  | TRL -- ^ Turkish lira
  | UAK -- ^ Ukrainian karbovanets
  | UGS -- ^ Ugandan shilling
  | UGW -- ^ Old Shilling
  | USS -- ^ United States dollar (same day) (funds code)
  | UYN -- ^ Uruguay peso
  | UYP -- ^ Uruguay new peso
  | VEB -- ^ Venezuelan bolívar
  | VEF -- ^ Venezuelan bolívar fuerte
  | VNC -- ^ Old Vietnamese dong
  | XEU -- ^ European Currency Unit
  | XFO -- ^ Gold franc (special settlement currency)
  | XFU -- ^ UIC franc (special settlement currency)
  | XRE -- ^ RINET funds code
  | YDD -- ^ South Yemeni dinar
  | YUD -- ^ Yugoslav dinar
  | YUM -- ^ Yugoslav dinar
  | YUN -- ^ Yugoslav dinar
  | ZAL -- ^ South African financial rand (funds code)
  | ZMK -- ^ Zambian kwacha
  | ZRN -- ^ Zairean new zaire
  | ZRZ -- ^ Zairean zaire
  | ZWC -- ^ Rhodesian dollar
  | ZWD -- ^ Zimbabwean dollar (first)
  | ZWN -- ^ Zimbabwean dollar (second)
  | ZWR -- ^ Zimbabwean dollar (third)
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
    AED -> Currency AED 784 2 "UAE Dirham" True
    AFN -> Currency AFN 971 2 "Afghani" True
    ALL -> Currency ALL 008 2 "Lek" True
    AMD -> Currency AMD 051 2 "Armenian Dram" True
    ANG -> Currency ANG 532 2 "Netherlands Antillean Guilder" True
    AOA -> Currency AOA 973 2 "Kwanza" True
    ARS -> Currency ARS 032 2 "Argentine Peso" True
    AUD -> Currency AUD 036 2 "Australian Dollar" True
    AWG -> Currency AWG 533 2 "Aruban Florin" True
    AZN -> Currency AZN 944 2 "Azerbaijan Manat" True
    BAM -> Currency BAM 977 2 "Convertible Mark" True
    BBD -> Currency BBD 052 2 "Barbados Dollar" True
    BDT -> Currency BDT 050 2 "Taka" True
    BGN -> Currency BGN 975 2 "Bulgarian Lev" True
    BHD -> Currency BHD 048 3 "Bahraini Dinar" True
    BIF -> Currency BIF 108 0 "Burundi Franc" True
    BMD -> Currency BMD 060 2 "Bermudian Dollar" True
    BND -> Currency BND 096 2 "Brunei Dollar" True
    BOB -> Currency BOB 068 2 "Boliviano" True
    BOV -> Currency BOV 984 2 "Mvdol" True
    BRL -> Currency BRL 986 2 "Brazilian Real" True
    BSD -> Currency BSD 044 2 "Bahamian Dollar" True
    BTN -> Currency BTN 064 2 "Ngultrum" True
    BWP -> Currency BWP 072 2 "Pula" True
    BYN -> Currency BYN 933 2 "Belarusian Ruble" True
    BZD -> Currency BZD 084 2 "Belize Dollar" True
    CAD -> Currency CAD 124 2 "Canadian Dollar" True
    CDF -> Currency CDF 976 2 "Congolese Franc" True
    CHE -> Currency CHE 947 2 "WIR Euro" True
    CHF -> Currency CHF 756 2 "Swiss Franc" True
    CHW -> Currency CHW 948 2 "WIR Franc" True
    CLF -> Currency CLF 990 4 "Unidad de Fomento" True
    CLP -> Currency CLP 152 0 "Chilean Peso" True
    CNY -> Currency CNY 156 2 "Yuan Renminbi" True
    COP -> Currency COP 170 2 "Colombian Peso" True
    COU -> Currency COU 970 2 "Unidad de Valor Real" True
    CRC -> Currency CRC 188 2 "Costa Rican Colon" True
    CUP -> Currency CUP 192 2 "Cuban Peso" True
    CVE -> Currency CVE 132 2 "Cabo Verde Escudo" True
    CZK -> Currency CZK 203 2 "Czech Koruna" True
    DJF -> Currency DJF 262 0 "Djibouti Franc" True
    DKK -> Currency DKK 208 2 "Danish Krone" True
    DOP -> Currency DOP 214 2 "Dominican Peso" True
    DZD -> Currency DZD 012 2 "Algerian Dinar" True
    EGP -> Currency EGP 818 2 "Egyptian Pound" True
    ERN -> Currency ERN 232 2 "Nakfa" True
    ETB -> Currency ETB 230 2 "Ethiopian Birr" True
    EUR -> Currency EUR 978 2 "Euro" True
    FJD -> Currency FJD 242 2 "Fiji Dollar" True
    FKP -> Currency FKP 238 2 "Falkland Islands Pound" True
    GBP -> Currency GBP 826 2 "Pound Sterling" True
    GEL -> Currency GEL 981 2 "Lari" True
    GHS -> Currency GHS 936 2 "Ghana Cedi" True
    GIP -> Currency GIP 292 2 "Gibraltar Pound" True
    GMD -> Currency GMD 270 2 "Dalasi" True
    GNF -> Currency GNF 324 0 "Guinean Franc" True
    GTQ -> Currency GTQ 320 2 "Quetzal" True
    GYD -> Currency GYD 328 2 "Guyana Dollar" True
    HKD -> Currency HKD 344 2 "Hong Kong Dollar" True
    HNL -> Currency HNL 340 2 "Lempira" True
    HTG -> Currency HTG 332 2 "Gourde" True
    HUF -> Currency HUF 348 2 "Forint" True
    IDR -> Currency IDR 360 2 "Rupiah" True
    ILS -> Currency ILS 376 2 "New Israeli Sheqel" True
    INR -> Currency INR 356 2 "Indian Rupee" True
    IQD -> Currency IQD 368 3 "Iraqi Dinar" True
    IRR -> Currency IRR 364 2 "Iranian Rial" True
    ISK -> Currency ISK 352 0 "Iceland Krona" True
    JMD -> Currency JMD 388 2 "Jamaican Dollar" True
    JOD -> Currency JOD 400 3 "Jordanian Dinar" True
    JPY -> Currency JPY 392 0 "Yen" True
    KES -> Currency KES 404 2 "Kenyan Shilling" True
    KGS -> Currency KGS 417 2 "Som" True
    KHR -> Currency KHR 116 2 "Riel" True
    KMF -> Currency KMF 174 0 "Comorian Franc" True
    KPW -> Currency KPW 408 2 "North Korean Won" True
    KRW -> Currency KRW 410 0 "Won" True
    KWD -> Currency KWD 414 3 "Kuwaiti Dinar" True
    KYD -> Currency KYD 136 2 "Cayman Islands Dollar" True
    KZT -> Currency KZT 398 2 "Tenge" True
    LAK -> Currency LAK 418 2 "Lao Kip" True
    LBP -> Currency LBP 422 2 "Lebanese Pound" True
    LKR -> Currency LKR 144 2 "Sri Lanka Rupee" True
    LRD -> Currency LRD 430 2 "Liberian Dollar" True
    LSL -> Currency LSL 426 2 "Loti" True
    LYD -> Currency LYD 434 3 "Libyan Dinar" True
    MAD -> Currency MAD 504 2 "Moroccan Dirham" True
    MDL -> Currency MDL 498 2 "Moldovan Leu" True
    MGA -> Currency MGA 969 2 "Malagasy Ariary" True
    MKD -> Currency MKD 807 2 "Denar" True
    MMK -> Currency MMK 104 2 "Kyat" True
    MNT -> Currency MNT 496 2 "Tugrik" True
    MOP -> Currency MOP 446 2 "Pataca" True
    MRU -> Currency MRU 929 2 "Ouguiya" True
    MUR -> Currency MUR 480 2 "Mauritius Rupee" True
    MVR -> Currency MVR 462 2 "Rufiyaa" True
    MWK -> Currency MWK 454 2 "Malawi Kwacha" True
    MXN -> Currency MXN 484 2 "Mexican Peso" True
    MXV -> Currency MXV 979 2 "Mexican Unidad de Inversion (UDI)" True
    MYR -> Currency MYR 458 2 "Malaysian Ringgit" True
    MZN -> Currency MZN 943 2 "Mozambique Metical" True
    NAD -> Currency NAD 516 2 "Namibia Dollar" True
    NGN -> Currency NGN 566 2 "Naira" True
    NIO -> Currency NIO 558 2 "Cordoba Oro" True
    NOK -> Currency NOK 578 2 "Norwegian Krone" True
    NPR -> Currency NPR 524 2 "Nepalese Rupee" True
    NZD -> Currency NZD 554 2 "New Zealand Dollar" True
    OMR -> Currency OMR 512 3 "Rial Omani" True
    PAB -> Currency PAB 590 2 "Balboa" True
    PEN -> Currency PEN 604 2 "Sol" True
    PGK -> Currency PGK 598 2 "Kina" True
    PHP -> Currency PHP 608 2 "Philippine Peso" True
    PKR -> Currency PKR 586 2 "Pakistan Rupee" True
    PLN -> Currency PLN 985 2 "Zloty" True
    PYG -> Currency PYG 600 0 "Guarani" True
    QAR -> Currency QAR 634 2 "Qatari Rial" True
    RON -> Currency RON 946 2 "Romanian Leu" True
    RSD -> Currency RSD 941 2 "Serbian Dinar" True
    RUB -> Currency RUB 643 2 "Russian Ruble" True
    RWF -> Currency RWF 646 0 "Rwanda Franc" True
    SAR -> Currency SAR 682 2 "Saudi Riyal" True
    SBD -> Currency SBD 090 2 "Solomon Islands Dollar" True
    SCR -> Currency SCR 690 2 "Seychelles Rupee" True
    SDG -> Currency SDG 938 2 "Sudanese Pound" True
    SEK -> Currency SEK 752 2 "Swedish Krona" True
    SGD -> Currency SGD 702 2 "Singapore Dollar" True
    SHP -> Currency SHP 654 2 "Saint-Helena Pound" True
    SLE -> Currency SLE 925 2 "Leone" True
    SLL -> Currency SLL 694 2 "Leone" True
    SOS -> Currency SOS 706 2 "Somali Shilling" True
    SRD -> Currency SRD 968 2 "Surinam Dollar" True
    SSP -> Currency SSP 728 2 "South Sudanese Pound" True
    STN -> Currency STN 930 2 "Dobra" True
    SVC -> Currency SVC 222 2 "El Salvador Colon" True
    SYP -> Currency SYP 760 2 "Syrian Pound" True
    SZL -> Currency SZL 748 2 "Lilangeni" True
    THB -> Currency THB 764 2 "Baht" True
    TJS -> Currency TJS 972 2 "Somoni" True
    TMT -> Currency TMT 934 2 "Turkmenistan New Manat" True
    TND -> Currency TND 788 3 "Tunisian Dinar" True
    TOP -> Currency TOP 776 2 "Pa’anga" True
    TRY -> Currency TRY 949 2 "Turkish Lira" True
    TTD -> Currency TTD 780 2 "Trinidad and Tobago Dollar" True
    TWD -> Currency TWD 901 2 "New Taiwan Dollar" True
    TZS -> Currency TZS 834 2 "Tanzanian Shilling" True
    UAH -> Currency UAH 980 2 "Hryvnia" True
    UGX -> Currency UGX 800 0 "Uganda" True
    USD -> Currency USD 840 2 "US Dollar" True
    USN -> Currency USN 997 2 "US Dollar (Next day)" True
    UYI -> Currency UYI 940 0 "Uruguay Peso en Unidades Indexadas (URUIURUI)" True
    UYU -> Currency UYU 858 2 "Peso Uruguayo" True
    UZS -> Currency UZS 860 2 "Uzbekistan" True
    VES -> Currency VES 928 2 "Bolívar" True
    VND -> Currency VND 704 0 "Dong" True
    VUV -> Currency VUV 548 0 "Vatu" True
    WST -> Currency WST 882 2 "Tala" True
    XAF -> Currency XAF 950 0 "CFA Franc BEAC" True
    XAG -> Currency XAG 961 0 "Silver" True
    XAU -> Currency XAU 959 0 "Gold" True
    XBA -> Currency XBA 955 0 "Bond Markets Unit European Composite Unit (EURCO)" True
    XBB -> Currency XBB 956 0 "Bond Markets Unit European Monetary Unit (E.M.U.-6)" True
    XBC -> Currency XBC 957 0 "Bond Markets Unit European Unit of Account 9 (E.U.A.-9)" True
    XBD -> Currency XBD 958 0 "Bond Markets Unit European Unit of Account 17 (E.U.A.-17)" True
    XCD -> Currency XCD 951 2 "East Caribbean Dollar" True
    XDR -> Currency XDR 960 0 "SDR (Special Drawing Right)" True
    XOF -> Currency XOF 952 0 "CFA Franc BCEAO" True
    XPD -> Currency XPD 964 0 "Palladium" True
    XPF -> Currency XPF 953 0 "CFP Franc" True
    XPT -> Currency XPT 962 0 "Platinum" True
    XSU -> Currency XSU 994 0 "Sucre" True
    XTS -> Currency XTS 963 0 "Codes specifically reserved for testing purposes" True
    XUA -> Currency XUA 965 0 "ADB Unit of Account" True
    XXX -> Currency XXX 999 0 "The codes assigned for transactions where no currency is involved" True
    YER -> Currency YER 886 2 "Yemeni Rial" True
    ZAR -> Currency ZAR 710 2 "Rand" True
    ZMW -> Currency ZMW 967 2 "Zambian Kwacha" True
    ZWL -> Currency ZWL 932 2 "Zimbabwe Dollar" True

    ALK -> Currency ALK 8 unknownMinorDigits "Old Albanian lek" False
    ARY -> Currency ARY 32 unknownMinorDigits "Argentine peso ley" False
    BEC -> Currency BEC 993 unknownMinorDigits "Belgian convertible franc (funds code)" False
    BEL -> Currency BEL 992 unknownMinorDigits "Belgian financial franc (funds code)" False
    BGJ -> Currency BGJ 100 unknownMinorDigits "Bulgarian lev (first)" False
    BGK -> Currency BGK 100 unknownMinorDigits "Bulgarian lev (second)" False
    ADP -> Currency ADP 20  0 "Andorran peseta" False
    AFA -> Currency AFA 4   2 "Afghan afghani" False
    AOK -> Currency AOK 24  0 "Angolan kwanza" False
    AON -> Currency AON 24  0 "Angolan novo kwanza" False
    AOR -> Currency AOR 982 0 "Angolan kwanza reajustado" False
    ARA -> Currency ARA 32  2 "Argentine austral" False
    ARP -> Currency ARP 32  2 "Argentine peso argentino" False
    ATS -> Currency ATS 40  2 "Austrian schilling" False
    AYM -> Currency AYM 945 0 "Azerbaijani manat" False
    AZM -> Currency AZM 31  2 "Azerbaijani manat" False
    BAD -> Currency BAD 70  2 "Bosnia and Herzegovina dinar" False
    BEF -> Currency BEF 56  2 "Belgian franc" False
    BGL -> Currency BGL 100 2 "Bulgarian lev (third)" False
    BOP -> Currency BOP 68  2 "Bolivian peso" False
    BRB -> Currency BRB 76  2 "Brazilian cruzeiro" False
    BRC -> Currency BRC 76  2 "Brazilian cruzado" False
    BRE -> Currency BRE 76  2 "Brazilian cruzeiro" False
    BRN -> Currency BRN 76  2 "Brazilian cruzado novo" False
    BRR -> Currency BRR 987 2 "Brazilian cruzeiro real" False
    BUK -> Currency BUK 104 unknownMinorDigits "Burmese kyat" False
    BYB -> Currency BYB 112 2 "Belarusian ruble" False
    BYR -> Currency BYR 974 0 "Belarusian ruble" False
    CHC -> Currency CHC 948 2 "WIR franc (for electronic currency)" False
    CSD -> Currency CSD 891 2 "Serbian dinar" False
    CSJ -> Currency CSJ 203 unknownMinorDigits "Czechoslovak koruna (second)" False
    CSK -> Currency CSK 200 unknownMinorDigits "Czechoslovak koruna" False
    CUC -> Currency CUC 931 2 "Cuban convertible peso" False
    CYP -> Currency CYP 196 2 "Cypriot pound" False
    DDM -> Currency DDM 278 unknownMinorDigits "East German mark" False
    DEM -> Currency DEM 276 2 "German mark" False
    ECS -> Currency ECS 218 0 "Ecuadorian sucre" False
    ECV -> Currency ECV 983 2 "Ecuador Unidad de Valor Constante (funds code)" False
    EEK -> Currency EEK 233 2 "Estonian kroon" False
    ESA -> Currency ESA 996 unknownMinorDigits "Spanish peseta (account A)" False
    ESB -> Currency ESB 995 unknownMinorDigits "Spanish peseta (account B)" False
    ESP -> Currency ESP 724 0 "Spanish peseta" False
    FIM -> Currency FIM 246 2 "Finnish markka" False
    FRF -> Currency FRF 250 2 "French franc" False
    GEK -> Currency GEK 268 0 "Georgian kuponi" False
    GHC -> Currency GHC 288 2 "Ghanaian cedi" False
    GHP -> Currency GHP 939 2 "Ghanaian cedi" False
    GNE -> Currency GNE 324 unknownMinorDigits "Guinean syli" False
    GNS -> Currency GNS 324 unknownMinorDigits "Guinean syli" False
    GQE -> Currency GQE 226 unknownMinorDigits "Equatorial Guinean ekwele" False
    GRD -> Currency GRD 300 unknownMinorDigits " Greek drachma" False
    GWE -> Currency GWE 624 unknownMinorDigits "Guinean escudo" False
    GWP -> Currency GWP 624 2 "Guinea-Bissau peso" False
    HRD -> Currency HRD 191 2 "Croatian dinar" False
    HRK -> Currency HRK 191 2 "Croatian kuna" False
    IEP -> Currency IEP 372 2 "Irish pound" False
    ILP -> Currency ILP 376 unknownMinorDigits " Israeli pound" False
    ILR -> Currency ILR 376 2 "Israeli shekel" False
    ISJ -> Currency ISJ 352 2 "Icelandic króna" False
    ITL -> Currency ITL 380 0 "Italian lira" False
    LTL -> Currency LTL 440 2 "Lithuanian litas" False
    LTT -> Currency LTT 440 2 "Lithuanian talonas" False
    LUF -> Currency LUF 442 2 "Luxembourg franc" False
    LVL -> Currency LVL 428 2 "Latvian lats" False
    LVR -> Currency LVR 428 2 "Latvian rublis" False
    MGF -> Currency MGF 450 0 "Malagasy franc" False
    MRO -> Currency MRO 478 2 "Mauritanian ouguiya" False
    MTL -> Currency MTL 470 2 "Maltese lira" False
    MZE -> Currency MZE 508 2 "Mozambican escudo" False
    MZM -> Currency MZM 508 2 "Mozambican metical" False
    NIC -> Currency NIC 558 2 "Nicaraguan córdoba" False
    NLG -> Currency NLG 528 2 "Dutch guilder" False
    PES -> Currency PES 604 2 "Peruvian sol" False
    PLZ -> Currency PLZ 616 2 "Polish zloty" False
    PTE -> Currency PTE 620 0 "Portuguese escudo" False
    RHD -> Currency RHD 716 2 "Rhodesian dollar" False
    ROL -> Currency ROL 642 0 "Romanian leu (third)" False
    RUR -> Currency RUR 810 2 "Russian ruble" False
    SDD -> Currency SDD 736 2 "Sudanese dinar" False
    SIT -> Currency SIT 705 2 "Slovenian tolar" False
    SKK -> Currency SKK 703 2 "Slovak koruna" False
    SRG -> Currency SRG 740 2 "Surinamese guilder" False
    STD -> Currency STD 678 2 "São Tomé and Príncipe dobra" False
    TJR -> Currency TJR 762 0 "Tajikistani ruble" False
    TMM -> Currency TMM 795 2 "Turkmenistani manat" False
    TPE -> Currency TPE 626 0 "Portuguese Timorese escudo" False
    TRL -> Currency TRL 792 0 "Turkish lira" False
    UAK -> Currency UAK 804 2 "Ukrainian karbovanets" False
    USS -> Currency USS 998 2 "United States dollar (same day) (funds code)" False
    UYN -> Currency UYN 858 2 "Uruguay peso" False
    VEB -> Currency VEB 862 2 "Venezuelan bolívar" False
    VEF -> Currency VEF 937 2 "Venezuelan bolívar fuerte" False
    XEU -> Currency XEU 954 0 "European Currency Unit" False
    YUD -> Currency YUD 890 2 "Yugoslav dinar" False
    YUM -> Currency YUM 891 2 "Yugoslav dinar" False
    YUN -> Currency YUN 890 2 "Yugoslav dinar" False
    ZAL -> Currency ZAL 991 2 "South African financial rand (funds code)" False
    ZMK -> Currency ZMK 894 2 "Zambian kwacha" False
    ZRN -> Currency ZRN 180 2 "Zairean new zaire" False
    ZRZ -> Currency ZRZ 180 2 "Zairean zaire" False
    ZWC -> Currency ZWC 716 2 "Rhodesian dollar" False
    ZWD -> Currency ZWD 716 2 "Zimbabwean dollar (first)" False
    ZWN -> Currency ZWN 942 2 "Zimbabwean dollar (second)" False
    ZWR -> Currency ZWR 935 2 "Zimbabwean dollar (third)" False
    LAJ -> Currency LAJ 418 unknownMinorDigits "Lao kip" False
    LSM -> Currency LSM 426 unknownMinorDigits "Lesotho loti" False
    LUC -> Currency LUC 989 unknownMinorDigits "Luxembourg convertible franc (funds code)" False
    LUL -> Currency LUL 988 unknownMinorDigits "Luxembourg financial franc (funds code)" False
    MLF -> Currency MLF 466 unknownMinorDigits "Malian franc" False
    MTP -> Currency MTP 470 unknownMinorDigits "Maltese pound" False
    MVQ -> Currency MVQ 462 unknownMinorDigits "Maldivian rupee" False
    MXP -> Currency MXP 484 unknownMinorDigits "Mexican peso" False
    PEH -> Currency PEH 604 unknownMinorDigits "Peruvian old sol" False
    PEI -> Currency PEI 604 unknownMinorDigits "Peruvian inti" False
    ROK -> Currency ROK 642 unknownMinorDigits "Romanian leu (second)" False
    SDP -> Currency SDP 736 unknownMinorDigits "Sudanese old pound" False
    UYP -> Currency UYP 858 unknownMinorDigits "Uruguay new peso" False
    SUR -> Currency SUR 810 unknownMinorDigits "Soviet Union ruble" False
    UGS -> Currency UGS 800 unknownMinorDigits "Ugandan shilling" False
    UGW -> Currency UGW 800 unknownMinorDigits "Old Shilling" False
    VNC -> Currency VNC 704 unknownMinorDigits "Old Vietnamese dong" False
    XFO -> Currency XFO unknownCodeXfo unknownMinorDigits "Gold franc (special settlement currency)" False
    XFU -> Currency XFU unknownCodeXfu unknownMinorDigits "UIC franc (special settlement currency)" False
    XRE -> Currency XRE unknownCodeXre unknownMinorDigits "RINET funds code" False
    YDD -> Currency YDD 720 unknownMinorDigits "South Yemeni dinar" False

-- | Value used when minor digits are unknown.
--
-- Minor digit numbers are not provided in ISO 4217's XLS file of currency codes
-- Where historical numbers are known, they are copied from Wikipedia's ISO 4217 page.
unknownMinorDigits :: Int 
unknownMinorDigits = 0

-- | No code for XFO exists in the ISO 4217's currency file. This is a placeholder value so that every currency can have a code.
unknownCodeXfo :: Int
unknownCodeXfo = 5000

-- | No code for XFU exists in the ISO 4217's currency file. This is a placeholder value so that every currency can have a code.
unknownCodeXfu :: Int
unknownCodeXfu = 5001

-- | No code for XRE exists in the ISO 4217's currency file. This is a placeholder value so that every currency can have a code.
unknownCodeXre :: Int
unknownCodeXre = 5002


-- | List of all currencies in the standard
currencies :: [Currency]
currencies =
  fmap fromAlpha [minBound..maxBound]


-- | List of all alpha constructors as strings
alphas :: [String]
alphas =
  fmap show $ dataTypeConstrs $ dataTypeOf (minBound :: Alpha)
