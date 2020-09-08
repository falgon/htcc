let types =
    https://raw.githubusercontent.com/falgon/dhall-docker-compose/master/compose/v3/types.dhall
let defaults =
    https://raw.githubusercontent.com/falgon/dhall-docker-compose/master/compose/v3/defaults.dhall

let htccService =
    defaults.Service // { 
        image = 
            Some "roki/htcc_example:1.0.0"
      , command = 
            Some (types.StringOrList.String "/bin/sh -c 'gcc -no-pie -o spec /htcc_work/spec.s && ./spec'")
      , volumes =
            Some
            [ "/tmp/htcc:/htcc_work"
            ]
      , build = 
            Some (types.Build.Object { 
                context = "."
              , dockerfile = "../docker/Dockerfile"
              , args = types.ListOrDict.List ([] : List (Optional types.StringOrNumber))
            })
    }

let services
    : types.Services
    = [ { mapKey = "htcc", mapValue = htccService } 
      ]

in  defaults.ComposeConfig 
    // { services = Some services } 
    : types.ComposeConfig
