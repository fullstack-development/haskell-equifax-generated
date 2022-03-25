#! /bin/bash

# https://openapi-generator.tech/docs/installation
openapi-generator-cli generate \
  -i '2020-09-21-Prescreen Of One.yaml' \
  -g haskell-http-client \
  -o. \
  --skip-validate-spec \
  --additional-properties=allowToJsonNulls=true \
  --additional-properties=allowFromJsonNulls=true \
  --additional-properties=allowNonUniqueOperationIds=true \
  --additional-properties=ensureUniqueParams=true \
  --additional-properties=useKatip=false \
  --additional-properties=cabalPackage=the-equifax-prescreen.cabal \
  --additional-properties=generateLenses=false
