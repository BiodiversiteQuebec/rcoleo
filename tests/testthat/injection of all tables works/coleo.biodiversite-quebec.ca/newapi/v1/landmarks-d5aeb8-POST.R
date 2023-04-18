structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/newapi/v1/landmarks", 
    status_code = 201L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Tue, 18 Apr 2023 19:58:37 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Transfer-Encoding` = "chunked", Connection = "keep-alive", 
        Location = "/landmarks?id=eq.31587", `Content-Range` = "*/*", 
        `Content-Profile` = "public"), class = "httr2_headers"), 
    body = charToRaw("[{\"id\":31587,\"campaign_id\":23432,\"tree_code\":null,\"taxa_name\":null,\"dbh\":null,\"dbh_unit\":null,\"azimut\":null,\"distance\":null,\"distance_unit\":null,\"geom\":{\"type\":\"Point\",\"coordinates\":[-73.81944,45.00642]},\"type\":\"gps\",\"notes\":null,\"created_at\":\"2023-04-18T15:58:37.264784-04:00\",\"updated_at\":\"2023-04-18T15:58:37.264784-04:00\",\"trap_id\":null,\"device_id\":14436,\"lure_id\":null,\"axis\":null,\"thermograph_type\":null}]")), class = "httr2_response")
