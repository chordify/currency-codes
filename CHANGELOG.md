# Changelog

## v3.0.0.0 (2018-03-13)

- Review Currency strictness
- Add deepseq's NFData instance for Currency & Alpha
- Manually write Show & Read instance for Alpha (compile-time & performance gain)
- Remove currency constructors in favor of helper `fromAlpha` 
- Add property tests to verify various instances 


## v2.0.0.0 (2017-09-11)

- Remove Swagger instances. It would be better to push them upstream to the swagger2 repository.


## v1.0.0.0 (2017-08-29)
  
- Provide 2 types `Currency` and `Alpha`
- Provide constructors for each currency and Alpha code listed in the standard
- Provide a list of all `Currency`
- Provide instances for:
  - Show
  - Eq
  - Read
  - Generic
  - Data
  - Typeable
  - FromJSON (aeson)
  - ToJSON   (aeson)
  - ToSchema (swagger2)
  - Val      (bson)
  - Random   (random)
  - Ord (Alpha only)
  - Enum (Alpha only
  - Bounded (Alpha only)
