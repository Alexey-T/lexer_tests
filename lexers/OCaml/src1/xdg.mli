(** Implement the XDG base directories specification

    http://standards.freedesktop.org/basedir-spec/basedir-spec-latest.html *)

(** The directory where the application should read/write config files. *)
val config_dir : string

(** The directory where the application should read/write data files. *)
val data_dir : string

(** The directory where the application should read/write cached files. *)
val cache_dir : string

(** The directroy where the application should store socket files. *)
val runtime_dir : string option
