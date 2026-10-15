# Databases and API requests — what's next

**Status:** 2026-10-15. The Databases page and the API page are built
(`ff4c878`, `873a475`), and the first seven items below have landed. The
rest is for later.

## Order

1. [x] **API: capture response values** (`# @capture token = $.token`) for
   later requests, as a login's token.
2. [x] **Databases: PostgreSQL**, the first server database. Passwords in the
   secrets store, outside the project.
3. [x] **SQL highlighting and completion**: a tree-sitter SQL grammar for
   `.sql` files and consoles; table and column names from the connection.
4. [x] **API: an Auth tab**: Bearer, Basic and API key, writing the header
   with the secret in a `{{variable}}`.
5. [x] **Databases: MySQL / MariaDB.**
6. [x] **API: form and multipart bodies**, with file uploads (curl `-F`).
7. [x] **API: cookies**, a jar for each environment.

## Later

### Databases

- [ ] Reopen table tabs and SQL consoles with the session.
- [ ] SQL console history, kept between sessions.
- [ ] SSL for PostgreSQL and MySQL (cl+ssl), and schemas other than the
      current one.
- [ ] Run a project `.sql` file from the Databases page.
- [ ] Offer the project's `*.db` / `*.sqlite` files when adding a connection.
- [ ] Export a whole table or query (not just the page) as CSV or JSON;
      import CSV.
- [ ] Blob cells: show as hex, save to a file, load from one.
- [ ] Foreign keys: choose a value from the other table; jump to the row.
- [ ] Make and change tables with a form.
- [ ] `CREATE TRIGGER … BEGIN … END` in the statement splitter.

### API requests

- [ ] Send a `.http` file's requests from its editor, with the response
      beside it.
- [ ] Find in a response; a JSON path or `jq` filter.
- [ ] Response checks (`# @expect status 200`) and running a whole file.
- [ ] Import Postman collections and OpenAPI specs.
- [ ] Proxy, client certificates, HTTP/2.
- [ ] The timing breakdown (DNS, connect, TLS, first byte) curl reports.
