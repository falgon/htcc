let types =
      https://raw.githubusercontent.com/falgon/dhall-docker-compose/2e37f35926a9306278f0fb41328ea6d3fed016ee/compose/v3/types.dhall
        sha256:df692daa4e2ec76fdf6eb873fdc703e995b74f45e63ed6b283b6cd4c3ac74a58

let defaults =
      https://raw.githubusercontent.com/falgon/dhall-docker-compose/2e37f35926a9306278f0fb41328ea6d3fed016ee/compose/v3/defaults.dhall
        sha256:205439311c2f3e48b8de6737322af5305966cbca5ceb34374699c4dda141da91

let htccService =
        defaults.Service
      ⫽ { image = Some "roki/htcc_test:1.0.0"
        , command = Some
            (types.StringOrList.String "/bin/bash /htcc_work/scripts/test.sh")
        , volumes = Some
          [ "/tmp/htcc:/htcc_work", "./docker/scripts:/htcc_work/scripts" ]
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
