structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/newapi/v1/cells", 
    status_code = 409L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Thu, 08 Jun 2023 14:10:39 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Transfer-Encoding` = "chunked", Connection = "keep-alive"), class = "httr2_headers"), 
    body = charToRaw("{\"hint\":null,\"details\":\"Key (name, cell_code)=(Doriath, FFF_ZZZ) already exists.\",\"code\":\"23505\",\"message\":\"duplicate key value violates unique constraint \\\"cells_name_cell_code_key\\\"\"}")), class = "httr2_response")
