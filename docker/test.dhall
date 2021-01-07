let types =
      https://raw.githubusercontent.com/falgon/dhall-docker-compose/master/compose/v3/types.dhall

let defaults =
      https://raw.githubusercontent.com/falgon/dhall-docker-compose/master/compose/v3/defaults.dhall

let htccService =
        defaults.Service
      ⫽ { image = Some "roki/htcc_test:1.0.0"
        , command = Some
            ( types.StringOrList.String
                "/bin/bash /htcc_work/scripts/test.sh"
            )
        , volumes = Some [ "/tmp/htcc:/htcc_work", "./docker/scripts:/htcc_work/scripts" ]
        , build = Some
            ( types.Build.Object
                { context = "."
                , dockerfile = "./docker/Dockerfile"
                , args =
                    types.ListOrDict.List
                      ([] : List (Optional types.StringOrNumber))
                }
            )
        }

let services
    : types.Services
    = [ { mapKey = "htcc", mapValue = htccService } ]

in  defaults.ComposeConfig ⫽ { services = Some services } : types.ComposeConfig
