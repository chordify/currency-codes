-- | A Currency representation as specified in the ISO-4217 standard

module Data.Currency where

import           Prelude       hiding (Ordering (..))

import           Control.Monad ((>=>))
import           Data.Aeson    as Aeson
import           Data.Aeson    (FromJSON (..), ToJSON (..))
import           Data.Bson     (Val (..), (=:))
import qualified Data.Bson     as Bson
import           Data.Data     (Data)
import           Data.Text     (Text)
import           Data.Typeable (Typeable)
import           GHC.Generics  (Generic)
import qualified Safe
import           System.Random (Random (..))


-- | Actual representation of a currency
data Currency = Currency
  { alpha   :: Alpha -- ^ Alpha Code
  , numeric :: Int   -- ^ Numeric code
  , minor   :: Int   -- ^ Number of decimal units
  , name    :: Text  -- ^ English name
  } deriving (Show, Eq, Read, Generic, Data, Typeable)

instance FromJSON Currency where parseJSON          = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON   Currency where toJSON             = Aeson.genericToJSON Aeson.defaultOptions

instance Val Currency where
  val Currency{..} = Bson.Doc
    [ "alpha"  =: alpha
    , "numeric"  =: numeric
    , "minor" =: minor
    , "name"    =: name
    ]

  cast' v = case v of
    Bson.Doc doc -> Currency
      <$> Bson.lookup "alpha"   doc
      <*> Bson.lookup "numeric" doc
      <*> Bson.lookup "minor"   doc
      <*> Bson.lookup "name"    doc

    _ -> Nothing

instance Random Currency where
  random g =
    let (i, g') = randomR (0, length currencies - 1) g
    in  (currencies !! i, g')

  randomR (a, b) g =
    let currencies' = dropWhile (/= a) $ takeWhile (/= b) currencies
        (i, g')    = randomR (0, length currencies' - 1) g
    in  (currencies' !! i, g')

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
  deriving (Show, Eq, Ord, Enum, Bounded, Read, Generic, Data, Typeable)

instance FromJSON Alpha where parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON   Alpha where toJSON    = Aeson.genericToJSON Aeson.defaultOptions

instance Val      Alpha where val       = val . show
                              cast'     = cast' >=> Safe.readMay

instance Random Alpha where
  random g =
    let (r, g') = randomR (fromEnum (minBound :: Alpha), fromEnum(maxBound :: Alpha)) g
    in  (toEnum r, g')

  randomR (l, h) g =
    let (r, g') = randomR (fromEnum l, fromEnum h) g
    in  (toEnum r, g')

-- Constructor for all currencies

unitOfAccount :: Currency
unitOfAccount =
  Currency XUA 965 0 "ADB Unit of Account"

afghani :: Currency
afghani =
  Currency AFN 971 2 "Afghani"

algerianDinar :: Currency
algerianDinar =
  Currency DZD 012 2 "Algerian Dinar"

argentinePeso :: Currency
argentinePeso =
  Currency ARS 032 2 "Argentine Peso"

armenianDram :: Currency
armenianDram =
  Currency AMD 051 2 "Armenian Dram"

arubanFlorin :: Currency
arubanFlorin =
  Currency AWG 533 2 "Aruban Florin"

australianDollar :: Currency
australianDollar =
  Currency AUD 036 2 "Australian Dollar"

azerbaijanManat :: Currency
azerbaijanManat =
  Currency AZN 944 2 "Azerbaijan Manat"

bahamianDollar :: Currency
bahamianDollar =
  Currency BSD 044 2 "Bahamian Dollar"

bahrainiDinar :: Currency
bahrainiDinar =
  Currency BHD 048 3 "Bahraini Dinar"

baht :: Currency
baht =
  Currency THB 764 2 "Baht"

balboa :: Currency
balboa =
  Currency PAB 590 2 "Balboa"

barbadosDollar :: Currency
barbadosDollar =
  Currency BBD 052 2 "Barbados Dollar"

belarusianRuble :: Currency
belarusianRuble =
  Currency BYN 933 2 "Belarusian Ruble"

belizeDollar :: Currency
belizeDollar =
  Currency BZD 084 2 "Belize Dollar"

bermudianDollar :: Currency
bermudianDollar =
  Currency BMD 060 2 "Bermudian Dollar"

boliviano :: Currency
boliviano =
  Currency BOB 068 2 "Boliviano"

bolívar :: Currency
bolívar =
  Currency VEF 937 2 "Bolívar"

bondMarketsCompositeUnit :: Currency
bondMarketsCompositeUnit =
  Currency XBA 955 0 "Bond Markets Unit European Composite Unit (EURCO)"

bondMarketsMonetaryUnit :: Currency
bondMarketsMonetaryUnit =
  Currency XBB 956 0 "Bond Markets Unit European Monetary Unit (E.M.U.-6)"

bondMarketsUnitofAccount17 :: Currency
bondMarketsUnitofAccount17 =
  Currency XBD 958 0 "Bond Markets Unit European Unit of Account 17 (E.U.A.-17)"

bondMarketsUnitofAccount9 :: Currency
bondMarketsUnitofAccount9 =
  Currency XBC 957 0 "Bond Markets Unit European Unit of Account 9 (E.U.A.-9)"

brazilianReal :: Currency
brazilianReal =
  Currency BRL 986 2 "Brazilian Real"

bruneiDollar :: Currency
bruneiDollar =
  Currency BND 096 2 "Brunei Dollar"

bulgarianLev :: Currency
bulgarianLev =
  Currency BGN 975 2 "Bulgarian Lev"

burundiFranc :: Currency
burundiFranc =
  Currency BIF 108 0 "Burundi Franc"

cfaFrancBCEAO :: Currency
cfaFrancBCEAO =
  Currency XOF 952 0 "CFA Franc BCEAO"

cfaFrancBEAC :: Currency
cfaFrancBEAC =
  Currency XAF 950 0 "CFA Franc BEAC"

cfpFranc :: Currency
cfpFranc =
  Currency XPF 953 0 "CFP Franc"

caboVerdeEscudo :: Currency
caboVerdeEscudo =
  Currency CVE 132 2 "Cabo Verde Escudo"

canadianDollar :: Currency
canadianDollar =
  Currency CAD 124 2 "Canadian Dollar"

caymanIslandsDollar :: Currency
caymanIslandsDollar =
  Currency KYD 136 2 "Cayman Islands Dollar"

chileanPeso :: Currency
chileanPeso =
  Currency CLP 152 0 "Chilean Peso"

testing :: Currency
testing =
  Currency XTS 963 0 "Codes specifically reserved for testing purposes"

colombianPeso :: Currency
colombianPeso =
  Currency COP 170 2 "Colombian Peso"

comorianFranc :: Currency
comorianFranc =
  Currency KMF 174 0 "Comorian Franc"

congoleseFranc :: Currency
congoleseFranc =
  Currency CDF 976 2 "Congolese Franc"

convertibleMark :: Currency
convertibleMark =
  Currency BAM 977 2 "Convertible Mark"

cordobaOro :: Currency
cordobaOro =
  Currency NIO 558 2 "Cordoba Oro"

costaRicanColon :: Currency
costaRicanColon =
  Currency CRC 188 2 "Costa Rican Colon"

cubanPeso :: Currency
cubanPeso =
  Currency CUP 192 2 "Cuban Peso"

czechKoruna :: Currency
czechKoruna =
  Currency CZK 203 2 "Czech Koruna"

dalasi :: Currency
dalasi =
  Currency GMD 270 2 "Dalasi"

danishKrone :: Currency
danishKrone =
  Currency DKK 208 2 "Danish Krone"

denar :: Currency
denar =
  Currency MKD 807 2 "Denar"

djiboutiFranc :: Currency
djiboutiFranc =
  Currency DJF 262 0 "Djibouti Franc"

dobra :: Currency
dobra =
  Currency STD 678 2 "Dobra"

dominicanPeso :: Currency
dominicanPeso =
  Currency DOP 214 2 "Dominican Peso"

dong :: Currency
dong =
  Currency VND 704 0 "Dong"

eastCaribbeanDollar :: Currency
eastCaribbeanDollar =
  Currency XCD 951 2 "East Caribbean Dollar"

egyptianPound :: Currency
egyptianPound =
  Currency EGP 818 2 "Egyptian Pound"

elSalvadorColon :: Currency
elSalvadorColon =
  Currency SVC 222 2 "El Salvador Colon"

ethiopianBirr :: Currency
ethiopianBirr =
  Currency ETB 230 2 "Ethiopian Birr"

euro :: Currency
euro =
  Currency EUR 978 2 "Euro"

falklandIslandsPound :: Currency
falklandIslandsPound =
  Currency FKP 238 2 "Falkland Islands Pound"

fijiDollar :: Currency
fijiDollar =
  Currency FJD 242 2 "Fiji Dollar"

forint :: Currency
forint =
  Currency HUF 348 2 "Forint"

ghanaCedi :: Currency
ghanaCedi =
  Currency GHS 936 2 "Ghana Cedi"

gibraltarPound :: Currency
gibraltarPound =
  Currency GIP 292 2 "Gibraltar Pound"

gold :: Currency
gold =
  Currency XAU 959 0 "Gold"

gourde :: Currency
gourde =
  Currency HTG 332 2 "Gourde"

guarani :: Currency
guarani =
  Currency PYG 600 0 "Guarani"

guineanFranc :: Currency
guineanFranc =
  Currency GNF 324 0 "Guinean Franc"

guyanaDollar :: Currency
guyanaDollar =
  Currency GYD 328 2 "Guyana Dollar"

hongKongDollar :: Currency
hongKongDollar =
  Currency HKD 344 2 "Hong Kong Dollar"

hryvnia :: Currency
hryvnia =
  Currency UAH 980 2 "Hryvnia"

icelandKrona :: Currency
icelandKrona =
  Currency ISK 352 0 "Iceland Krona"

indianRupee :: Currency
indianRupee =
  Currency INR 356 2 "Indian Rupee"

iranianRial :: Currency
iranianRial =
  Currency IRR 364 2 "Iranian Rial"

iraqiDinar :: Currency
iraqiDinar =
  Currency IQD 368 3 "Iraqi Dinar"

jamaicanDollar :: Currency
jamaicanDollar =
  Currency JMD 388 2 "Jamaican Dollar"

jordanianDinar :: Currency
jordanianDinar =
  Currency JOD 400 3 "Jordanian Dinar"

kenyanShilling :: Currency
kenyanShilling =
  Currency KES 404 2 "Kenyan Shilling"

kina :: Currency
kina =
  Currency PGK 598 2 "Kina"

kuna :: Currency
kuna =
  Currency HRK 191 2 "Kuna"

kuwaitiDinar :: Currency
kuwaitiDinar =
  Currency KWD 414 3 "Kuwaiti Dinar"

kwanza :: Currency
kwanza =
  Currency AOA 973 2 "Kwanza"

kyat :: Currency
kyat =
  Currency MMK 104 2 "Kyat"

laoKip :: Currency
laoKip =
  Currency LAK 418 2 "Lao Kip"

lari :: Currency
lari =
  Currency GEL 981 2 "Lari"

lebanesePound :: Currency
lebanesePound =
  Currency LBP 422 2 "Lebanese Pound"

lek :: Currency
lek =
  Currency ALL 008 2 "Lek"

lempira :: Currency
lempira =
  Currency HNL 340 2 "Lempira"

leone :: Currency
leone =
  Currency SLL 694 2 "Leone"

liberianDollar :: Currency
liberianDollar =
  Currency LRD 430 2 "Liberian Dollar"

libyanDinar :: Currency
libyanDinar =
  Currency LYD 434 3 "Libyan Dinar"

lilangeni :: Currency
lilangeni =
  Currency SZL 748 2 "Lilangeni"

loti :: Currency
loti =
  Currency LSL 426 2 "Loti"

malagasyAriary :: Currency
malagasyAriary =
  Currency MGA 969 2 "Malagasy Ariary"

malawiKwacha :: Currency
malawiKwacha =
  Currency MWK 454 2 "Malawi Kwacha"

malaysianRinggit :: Currency
malaysianRinggit =
  Currency MYR 458 2 "Malaysian Ringgit"

mauritiusRupee :: Currency
mauritiusRupee =
  Currency MUR 480 2 "Mauritius Rupee"

mexicanPeso :: Currency
mexicanPeso =
  Currency MXN 484 2 "Mexican Peso"

mexicanUnidadDeInversion :: Currency
mexicanUnidadDeInversion =
  Currency MXV 979 2 "Mexican Unidad de Inversion (UDI)"

moldovanLeu :: Currency
moldovanLeu =
  Currency MDL 498 2 "Moldovan Leu"

moroccanDirham :: Currency
moroccanDirham =
  Currency MAD 504 2 "Moroccan Dirham"

mozambiqueMetical :: Currency
mozambiqueMetical =
  Currency MZN 943 2 "Mozambique Metical"

mvdol :: Currency
mvdol =
  Currency BOV 984 2 "Mvdol"

naira :: Currency
naira =
  Currency NGN 566 2 "Naira"

nakfa :: Currency
nakfa =
  Currency ERN 232 2 "Nakfa"

namibiaDollar :: Currency
namibiaDollar =
  Currency NAD 516 2 "Namibia Dollar"

nepaleseRupee :: Currency
nepaleseRupee =
  Currency NPR 524 2 "Nepalese Rupee"

netherlandsAntilleanGuilder :: Currency
netherlandsAntilleanGuilder =
  Currency ANG 532 2 "Netherlands Antillean Guilder"

newIsraeliSheqel :: Currency
newIsraeliSheqel =
  Currency ILS 376 2 "New Israeli Sheqel"

newTaiwanDollar :: Currency
newTaiwanDollar =
  Currency TWD 901 2 "New Taiwan Dollar"

newZealandDollar :: Currency
newZealandDollar =
  Currency NZD 554 2 "New Zealand Dollar"

ngultrum :: Currency
ngultrum =
  Currency BTN 064 2 "Ngultrum"

northKoreanWon :: Currency
northKoreanWon =
  Currency KPW 408 2 "North Korean Won"

norwegianKrone :: Currency
norwegianKrone =
  Currency NOK 578 2 "Norwegian Krone"

ouguiya :: Currency
ouguiya =
  Currency MRO 478 2 "Ouguiya"

pakistanRupee :: Currency
pakistanRupee =
  Currency PKR 586 2 "Pakistan Rupee"

palladium :: Currency
palladium =
  Currency XPD 964 0 "Palladium"

pataca :: Currency
pataca =
  Currency MOP 446 2 "Pataca"

paanga :: Currency
paanga =
  Currency TOP 776 2 "Pa’anga"

pesoConvertible :: Currency
pesoConvertible =
  Currency CUC 931 2 "Peso Convertible"

pesoUruguayo :: Currency
pesoUruguayo =
  Currency UYU 858 2 "Peso Uruguayo"

philippinePeso :: Currency
philippinePeso =
  Currency PHP 608 2 "Philippine Peso"

platinum :: Currency
platinum =
  Currency XPT 962 0 "Platinum"

poundSterling :: Currency
poundSterling =
  Currency GBP 826 2 "Pound Sterling"

pula :: Currency
pula =
  Currency BWP 072 2 "Pula"

qatariRial :: Currency
qatariRial =
  Currency QAR 634 2 "Qatari Rial"

quetzal :: Currency
quetzal =
  Currency GTQ 320 2 "Quetzal"

rand :: Currency
rand =
  Currency ZAR 710 2 "Rand"

rialOmani :: Currency
rialOmani =
  Currency OMR 512 3 "Rial Omani"

riel :: Currency
riel =
  Currency KHR 116 2 "Riel"

romanianLeu :: Currency
romanianLeu =
  Currency RON 946 2 "Romanian Leu"

rufiyaa :: Currency
rufiyaa =
  Currency MVR 462 2 "Rufiyaa"

rupiah :: Currency
rupiah =
  Currency IDR 360 2 "Rupiah"

russianRuble :: Currency
russianRuble =
  Currency RUB 643 2 "Russian Ruble"

rwandaFranc :: Currency
rwandaFranc =
  Currency RWF 646 0 "Rwanda Franc"

sdr :: Currency
sdr =
  Currency XDR 960 0 "SDR (Special Drawing Right)"

saintHelenaPound :: Currency
saintHelenaPound =
  Currency SHP 654 2 "Saint-Helena Pound"

saudiRiyal :: Currency
saudiRiyal =
  Currency SAR 682 2 "Saudi Riyal"

serbianDinar :: Currency
serbianDinar =
  Currency RSD 941 2 "Serbian Dinar"

seychellesRupee :: Currency
seychellesRupee =
  Currency SCR 690 2 "Seychelles Rupee"

silver :: Currency
silver =
  Currency XAG 961 0 "Silver"

singaporeDollar :: Currency
singaporeDollar =
  Currency SGD 702 2 "Singapore Dollar"

sol :: Currency
sol =
  Currency PEN 604 2 "Sol"

solomonIslandsDollar :: Currency
solomonIslandsDollar =
  Currency SBD 090 2 "Solomon Islands Dollar"

som :: Currency
som =
  Currency KGS 417 2 "Som"

somaliShilling :: Currency
somaliShilling =
  Currency SOS 706 2 "Somali Shilling"

somoni :: Currency
somoni =
  Currency TJS 972 2 "Somoni"

southSudanesePound :: Currency
southSudanesePound =
  Currency SSP 728 2 "South Sudanese Pound"

sriLankaRupee :: Currency
sriLankaRupee =
  Currency LKR 144 2 "Sri Lanka Rupee"

sucre :: Currency
sucre =
  Currency XSU 994 0 "Sucre"

sudanesePound :: Currency
sudanesePound =
  Currency SDG 938 2 "Sudanese Pound"

surinamDollar :: Currency
surinamDollar =
  Currency SRD 968 2 "Surinam Dollar"

swedishKrona :: Currency
swedishKrona =
  Currency SEK 752 2 "Swedish Krona"

swissFranc :: Currency
swissFranc =
  Currency CHF 756 2 "Swiss Franc"

syrianPound :: Currency
syrianPound =
  Currency SYP 760 2 "Syrian Pound"

taka :: Currency
taka =
  Currency BDT 050 2 "Taka"

tala :: Currency
tala =
  Currency WST 882 2 "Tala"

tanzanianShilling :: Currency
tanzanianShilling =
  Currency TZS 834 2 "Tanzanian Shilling"

tenge :: Currency
tenge =
  Currency KZT 398 2 "Tenge"

noCurrency :: Currency
noCurrency =
  Currency XXX 999 0 "The codes assigned for transactions where no currency is involved"

trinidadAndTobagoDollar :: Currency
trinidadAndTobagoDollar =
  Currency TTD 780 2 "Trinidad and Tobago Dollar"

tugrik :: Currency
tugrik =
  Currency MNT 496 2 "Tugrik"

tunisianDinar :: Currency
tunisianDinar =
  Currency TND 788 3 "Tunisian Dinar"

turkishLira :: Currency
turkishLira =
  Currency TRY 949 2 "Turkish Lira"

turkmenistanNewManat :: Currency
turkmenistanNewManat =
  Currency TMT 934 2 "Turkmenistan New Manat"

uAEDirham :: Currency
uAEDirham =
  Currency AED 784 2 "UAE Dirham"

usDollar :: Currency
usDollar =
  Currency USD 840 2 "US Dollar"

usDollarNextday :: Currency
usDollarNextday =
  Currency USN 997 2 "US Dollar (Next day)"

ugandaShilling :: Currency
ugandaShilling =
  Currency UGX 800 0 "Uganda"

unidaddeFomento :: Currency
unidaddeFomento =
  Currency CLF 990 4 "Unidad de Fomento"

unidaddeValorReal :: Currency
unidaddeValorReal =
  Currency COU 970 2 "Unidad de Valor Real"

uruguayPesoEnUnidadesIndexadas :: Currency
uruguayPesoEnUnidadesIndexadas =
  Currency UYI 940 0 "Uruguay Peso en Unidades Indexadas (URUIURUI)"

uzbekistanSum :: Currency
uzbekistanSum =
  Currency UZS 860 2 "Uzbekistan"

vatu :: Currency
vatu =
  Currency VUV 548 0 "Vatu"

wirEuro :: Currency
wirEuro =
  Currency CHE 947 2 "WIR Euro"

wirFranc :: Currency
wirFranc =
  Currency CHW 948 2 "WIR Franc"

won :: Currency
won =
  Currency KRW 410 0 "Won"

yemeniRial :: Currency
yemeniRial =
  Currency YER 886 2 "Yemeni Rial"

yen :: Currency
yen =
  Currency JPY 392 0 "Yen"

yuanRenminbi :: Currency
yuanRenminbi =
  Currency CNY 156 2 "Yuan Renminbi"

zambianKwacha :: Currency
zambianKwacha =
  Currency ZMW 967 2 "Zambian Kwacha"

zimbabweDollar :: Currency
zimbabweDollar =
  Currency ZWL 932 2 "Zimbabwe Dollar"

zloty :: Currency
zloty =
  Currency PLN 985 2 "Zloty"

-- | List of all currencies in the standard
currencies :: [Currency]
currencies =
  [ unitOfAccount
  , afghani
  , algerianDinar
  , argentinePeso
  , armenianDram
  , arubanFlorin
  , australianDollar
  , azerbaijanManat
  , bahamianDollar
  , bahrainiDinar
  , baht
  , balboa
  , barbadosDollar
  , belarusianRuble
  , belizeDollar
  , bermudianDollar
  , boliviano
  , bolívar
  , bondMarketsCompositeUnit
  , bondMarketsMonetaryUnit
  , bondMarketsUnitofAccount17
  , bondMarketsUnitofAccount9
  , brazilianReal
  , bruneiDollar
  , bulgarianLev
  , burundiFranc
  , cfaFrancBCEAO
  , cfaFrancBEAC
  , cfpFranc
  , caboVerdeEscudo
  , canadianDollar
  , caymanIslandsDollar
  , chileanPeso
  , testing
  , colombianPeso
  , comorianFranc
  , congoleseFranc
  , convertibleMark
  , cordobaOro
  , costaRicanColon
  , cubanPeso
  , czechKoruna
  , dalasi
  , danishKrone
  , denar
  , djiboutiFranc
  , dobra
  , dominicanPeso
  , dong
  , eastCaribbeanDollar
  , egyptianPound
  , elSalvadorColon
  , ethiopianBirr
  , euro
  , falklandIslandsPound
  , fijiDollar
  , forint
  , ghanaCedi
  , gibraltarPound
  , gold
  , gourde
  , guarani
  , guineanFranc
  , guyanaDollar
  , hongKongDollar
  , hryvnia
  , icelandKrona
  , indianRupee
  , iranianRial
  , iraqiDinar
  , jamaicanDollar
  , jordanianDinar
  , kenyanShilling
  , kina
  , kuna
  , kuwaitiDinar
  , kwanza
  , kyat
  , laoKip
  , lari
  , lebanesePound
  , lek
  , lempira
  , leone
  , liberianDollar
  , libyanDinar
  , lilangeni
  , loti
  , malagasyAriary
  , malawiKwacha
  , malaysianRinggit
  , mauritiusRupee
  , mexicanPeso
  , mexicanUnidadDeInversion
  , moldovanLeu
  , moroccanDirham
  , mozambiqueMetical
  , mvdol
  , naira
  , nakfa
  , namibiaDollar
  , nepaleseRupee
  , netherlandsAntilleanGuilder
  , newIsraeliSheqel
  , newTaiwanDollar
  , newZealandDollar
  , ngultrum
  , northKoreanWon
  , norwegianKrone
  , ouguiya
  , pakistanRupee
  , palladium
  , pataca
  , paanga
  , pesoConvertible
  , pesoUruguayo
  , philippinePeso
  , platinum
  , poundSterling
  , pula
  , qatariRial
  , quetzal
  , rand
  , rialOmani
  , riel
  , romanianLeu
  , rufiyaa
  , rupiah
  , russianRuble
  , rwandaFranc
  , sdr
  , saintHelenaPound
  , saudiRiyal
  , serbianDinar
  , seychellesRupee
  , silver
  , singaporeDollar
  , sol
  , solomonIslandsDollar
  , som
  , somaliShilling
  , somoni
  , southSudanesePound
  , sriLankaRupee
  , sucre
  , sudanesePound
  , surinamDollar
  , swedishKrona
  , swissFranc
  , syrianPound
  , taka
  , tala
  , tanzanianShilling
  , tenge
  , noCurrency
  , trinidadAndTobagoDollar
  , tugrik
  , tunisianDinar
  , turkishLira
  , turkmenistanNewManat
  , uAEDirham
  , usDollar
  , usDollarNextday
  , ugandaShilling
  , unidaddeFomento
  , unidaddeValorReal
  , uruguayPesoEnUnidadesIndexadas
  , uzbekistanSum
  , vatu
  , wirEuro
  , wirFranc
  , won
  , yemeniRial
  , yen
  , yuanRenminbi
  , zambianKwacha
  , zloty
  , zimbabweDollar
  ]
