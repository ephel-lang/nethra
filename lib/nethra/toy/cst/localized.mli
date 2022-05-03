type 'a t = Localized of 'a * Nethra_syntax_source.Region.t

val region : 'a t -> Nethra_syntax_source.Region.t
