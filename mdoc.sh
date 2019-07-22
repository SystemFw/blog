coursier launch org.scalameta:mdoc_2.12:1.3.1 \
         co.fs2:fs2-core_2.12:1.0.5 \
         io.circe:circe-core_2.12:0.11.1 \
         io.circe:circe-generic_2.12:0.11.1 \
         io.circe:circe-parser_2.12:0.11.1 -- \
         --in content/staging \
         --out content/posts \
         --watch
