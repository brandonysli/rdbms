type object_phrase = string list
(** The type [object_phrase] represents the object phrase that can be part of a
    SQL command. Each element of the list represents a word of the object
    phrase, where a "word" is defined as a consecutive sequence of non-space
    characters. Thus, no element of the list should contain any leading,
    internal, or trailing spaces. The list is in the same order as the words in
    the original command *)
