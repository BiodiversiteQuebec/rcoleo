structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/api/v1/lures", 
    status_code = 500L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Thu, 18 Aug 2022 17:29:08 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Content-Length` = "136", Connection = "keep-alive", 
        `X-Powered-By` = "Express", `Access-Control-Allow-Origin` = "*", 
        ETag = "W/\"88-Wo2y+dbRcf6ZhrwM9uHjL7/1hE8\""), class = "httr2_headers"), 
    body = charToRaw("{\"message\":\"internal error\",\"errors\":[\"insert or update on table \\\"lures\\\" violates foreign key constraint \\\"lures_campaign_id_fkey\\\"\"]}")), class = "httr2_response")
