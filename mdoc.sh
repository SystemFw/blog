coursier launch org.scalameta:mdoc_2.13:2.2.20 -- \
         --classpath $(coursier fetch -p co.fs2:fs2-core_2.13:3.0.2) \
         --in content/staging \
         --out content/posts \
         --watch
