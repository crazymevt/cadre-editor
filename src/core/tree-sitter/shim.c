/* shim.c — tree-sitter for Cadre, through plain integers and pointers
 *
 * tree-sitter's API passes nodes and points by value, which CFFI can't do
 * without libffi. These functions take and return only pointers and
 * integers: parsing, edits, queries (matches as flat arrays of integers),
 * and the multi-line nodes a tree has, for folding. Cadre compiles this
 * once, next to the grammars it installs, and links it to libtree-sitter.
 */

#include <tree_sitter/api.h>
#include <stdint.h>

/* Parsers and trees */

TSParser *cts_parser_new(const TSLanguage *language) {
  TSParser *parser = ts_parser_new();
  if (!ts_parser_set_language(parser, language)) {
    ts_parser_delete(parser);
    return 0;
  }
  return parser;
}

void cts_parser_delete(TSParser *parser) { ts_parser_delete(parser); }

uint32_t cts_language_abi(const TSLanguage *language) { return ts_language_abi_version(language); }

TSTree *cts_parse(TSParser *parser, TSTree *old, const char *text, uint32_t length) {
  return ts_parser_parse_string(parser, old, text, length);
}

void cts_tree_delete(TSTree *tree) { ts_tree_delete(tree); }

void cts_tree_edit(TSTree *tree, uint32_t start_byte, uint32_t old_end_byte, uint32_t new_end_byte,
                   uint32_t start_row, uint32_t start_column, uint32_t old_end_row, uint32_t old_end_column,
                   uint32_t new_end_row, uint32_t new_end_column) {
  TSInputEdit edit = {start_byte, old_end_byte, new_end_byte,
                      {start_row, start_column}, {old_end_row, old_end_column}, {new_end_row, new_end_column}};
  ts_tree_edit(tree, &edit);
}

uint32_t cts_tree_has_error(TSTree *tree) { return ts_node_has_error(ts_tree_root_node(tree)); }

/* Queries */

TSQuery *cts_query_new(const TSLanguage *language, const char *source, uint32_t length,
                       uint32_t *error_offset, uint32_t *error_type) {
  TSQueryError error = TSQueryErrorNone;
  TSQuery *query = ts_query_new(language, source, length, error_offset, &error);
  *error_type = (uint32_t)error;
  return query;
}

void cts_query_delete(TSQuery *query) { ts_query_delete(query); }
uint32_t cts_query_pattern_count(TSQuery *query) { return ts_query_pattern_count(query); }
uint32_t cts_query_capture_count(TSQuery *query) { return ts_query_capture_count(query); }
uint32_t cts_query_string_count(TSQuery *query) { return ts_query_string_count(query); }

const char *cts_query_capture_name(TSQuery *query, uint32_t index, uint32_t *length) {
  return ts_query_capture_name_for_id(query, index, length);
}

const char *cts_query_string(TSQuery *query, uint32_t index, uint32_t *length) {
  return ts_query_string_value_for_id(query, index, length);
}

/* The predicate steps of PATTERN, as (type value-id) pairs into OUT (type 0
   ends a predicate, 1 is a capture, 2 a string). Returns the number of steps,
   which may be more than fit in MAX pairs. */
uint32_t cts_query_predicates(TSQuery *query, uint32_t pattern, uint32_t *out, uint32_t max) {
  uint32_t count = 0;
  const TSQueryPredicateStep *steps = ts_query_predicates_for_pattern(query, pattern, &count);
  for (uint32_t i = 0; i < count && i < max; i++) {
    out[2 * i] = (uint32_t)steps[i].type;
    out[2 * i + 1] = steps[i].value_id;
  }
  return count;
}

/* The matches of QUERY on TREE between the bytes START and END, into OUT as
   records: pattern, capture count, then (capture id, start byte, end byte)
   for each capture. Returns the number of words written; a match that would
   not fit stops the scan, and *TRUNCATED is set. */
uint32_t cts_matches(TSTree *tree, TSQuery *query, uint32_t start, uint32_t end,
                     uint32_t *out, uint32_t max, uint32_t *truncated) {
  TSQueryCursor *cursor = ts_query_cursor_new();
  ts_query_cursor_set_byte_range(cursor, start, end);
  ts_query_cursor_exec(cursor, query, ts_tree_root_node(tree));
  TSQueryMatch match;
  uint32_t n = 0;
  *truncated = 0;
  while (ts_query_cursor_next_match(cursor, &match)) {
    uint32_t need = 2 + 3 * (uint32_t)match.capture_count;
    if (n + need > max) { *truncated = 1; break; }
    out[n++] = match.pattern_index;
    out[n++] = match.capture_count;
    for (uint16_t i = 0; i < match.capture_count; i++) {
      TSNode node = match.captures[i].node;
      out[n++] = match.captures[i].index;
      out[n++] = ts_node_start_byte(node);
      out[n++] = ts_node_end_byte(node);
    }
  }
  ts_query_cursor_delete(cursor);
  return n;
}

/* Named nodes that span lines, as (first row, last row) pairs into OUT, at
   most MAX pairs. A node ending at a line's start ends on the line before. */
uint32_t cts_multiline_nodes(TSTree *tree, uint32_t *out, uint32_t max) {
  TSTreeCursor cursor = ts_tree_cursor_new(ts_tree_root_node(tree));
  uint32_t n = 0;
  for (;;) {
    TSNode node = ts_tree_cursor_current_node(&cursor);
    TSPoint start = ts_node_start_point(node), end = ts_node_end_point(node);
    uint32_t last = (end.column == 0 && end.row > 0) ? end.row - 1 : end.row;
    int spans = last > start.row;
    /* Not the root: it is the whole text. */
    if (spans && ts_node_is_named(node) && ts_tree_cursor_current_depth(&cursor) > 0 && n < max) {
      out[2 * n] = start.row;
      out[2 * n + 1] = last;
      n++;
    }
    /* Only nodes that span lines can hold nodes that do. */
    if (spans && ts_tree_cursor_goto_first_child(&cursor)) continue;
    if (ts_tree_cursor_goto_next_sibling(&cursor)) continue;
    for (;;) {
      if (!ts_tree_cursor_goto_parent(&cursor)) { ts_tree_cursor_delete(&cursor); return n; }
      if (ts_tree_cursor_goto_next_sibling(&cursor)) break;
    }
  }
}
