structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/api/v1/taxa", 
    status_code = 400L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Wed, 17 Aug 2022 18:44:23 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Content-Length` = "90", Connection = "keep-alive", `X-Powered-By` = "Express", 
        `Access-Control-Allow-Origin` = "*", ETag = "W/\"5a-dw/JCzO62q/5EmQOdzU3BAQ8OhQ\""), class = "httr2_headers"), 
    body = charToRaw("{\"message\":\"Validation error\",\"errors\":[{\"field\":\"name\",\"message\":\"name must be unique\"}]}")), class = "httr2_response")
