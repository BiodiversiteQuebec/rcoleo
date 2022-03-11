structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/api/v1/sites", 
    status_code = 400L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Thu, 10 Mar 2022 19:36:45 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Content-Length` = "133", Connection = "keep-alive", 
        `X-Powered-By` = "Express", `Access-Control-Allow-Origin` = "*", 
        ETag = "W/\"85-zzeAojQv8gvxLSbb5QefrHHJkeA\""), class = "httr2_headers"), 
    body = charToRaw("{\"message\":\"notNull Violation: sites.cell_id cannot be null\",\"errors\":[{\"field\":\"cell_id\",\"message\":\"sites.cell_id cannot be null\"}]}")), class = "httr2_response")
