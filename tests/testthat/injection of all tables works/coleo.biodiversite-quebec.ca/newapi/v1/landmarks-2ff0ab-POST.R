structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/newapi/v1/landmarks", 
    status_code = 201L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Mon, 06 Feb 2023 14:56:18 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Transfer-Encoding` = "chunked", Connection = "keep-alive", 
        Location = "/landmarks?id=eq.23011", `Content-Range` = "*/*", 
        `Content-Profile` = "public"), class = "httr2_headers"), 
    body = charToRaw("[{\"id\":23011,\"campaign_id\":15495,\"tree_code\":null,\"taxa_name\":null,\"dbh\":null,\"dbh_unit\":null,\"azimut\":null,\"distance\":null,\"distance_unit\":null,\"geom\":{\"type\":\"Point\",\"coordinates\":[-73.81944,45.00642]},\"type\":\"gps\",\"notes\":null,\"created_at\":\"2023-02-06T09:56:17.966203-05:00\",\"updated_at\":\"2023-02-06T09:56:17.966203-05:00\",\"trap_id\":null,\"device_id\":7938,\"lure_id\":null,\"axis\":null,\"thermograph_type\":null}]")), class = "httr2_response")
