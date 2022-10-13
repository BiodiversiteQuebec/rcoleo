structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/api/v1/obs_species", 
    status_code = 400L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Wed, 07 Sep 2022 18:00:57 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Content-Length` = "112", Connection = "keep-alive", 
        `X-Powered-By` = "Express", `Access-Control-Allow-Origin` = "*", 
        ETag = "W/\"70-MXuZocTQ7avGy0tHt3CRFdfw7QM\""), class = "httr2_headers"), 
    body = charToRaw("{\"message\":\"Validation error\",\"errors\":[{\"field\":\"scientific_name\",\"message\":\"scientific_name must be unique\"}]}")), class = "httr2_response")
