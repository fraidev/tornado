type piece_block =
  { index : int
  ; start : int
  ; length : int
  }

type t =
  { torrent : Torrent.t
  ; queue : piece_block Queue.t
  ; choked : bool
  }

let create torrent = { torrent; queue = Queue.create (); choked = true }

let queue t piece_block =
  let n_blocks =
    Torrent.calculate_piece_size t.torrent t.torrent.piece_length
  in
  let counter = ref 0 in
  while
    incr counter;
    !counter <> n_blocks
  do
    Queue.add piece_block t.queue
  done
;;

let deque t = Queue.take t.queue
let peek t = Queue.peek t.queue
let length t = Queue.length t.queue
