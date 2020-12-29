(*
 * Copyright (c) 2016 Docker Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

(** https://msdn.microsoft.com/en-us/library/windows/desktop/aa365588(v=vs.85).aspx *)

(** Raised on platforms which don't support named pipes *)
exception Not_available

module Server : sig
  (** The server should create a named pipe at a particular path under \\.\pipe *)
  val create : string -> Unix.file_descr

  (** Connect blocks until a client connects to this named pipe *)
  val connect : Unix.file_descr -> unit

  (** Flushes outstanding write buffers, typically called before disconnect *)
  val flush : Unix.file_descr -> unit

  (** Disconnects the connected client *)
  val disconnect : Unix.file_descr -> unit

  (** Removes the underlying OS resource *)
  val destroy : Unix.file_descr -> unit
end

(* https://msdn.microsoft.com/en-gb/library/windows/desktop/aa365592(v=vs.85).aspx *)

module Client : sig
  (** A pipe is busy if the server is not ready to accept more connections:
      perhaps it is still forking a thread for the previous one. A client should
      call the wait function to wait for a free slot. *)
  exception Pipe_busy

  (** Connect to the named pipe server on the given path (e.g. \\.\pipe\foo). If
      the server isn't running then this raises Unix_error(Unix.ENOENT...). If
      the server isn't ready to accept a connection then this raises Pipe_busy,
      and the client should call [wait]. *)
  val openpipe : string -> Unix.file_descr

  (** [wait path ms] wait for up to [ms] milliseconds for the server to become
      available. Returns when the server has a free slot: in this case the
      client should call [openpipe] again. *)
  val wait : string -> int -> unit
end
