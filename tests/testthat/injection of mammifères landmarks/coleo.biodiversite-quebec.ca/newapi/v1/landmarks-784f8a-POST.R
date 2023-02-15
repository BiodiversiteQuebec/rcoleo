structure(list(method = "POST", url = "https://coleo.biodiversite-quebec.ca/newapi/v1/landmarks", 
    status_code = 201L, headers = structure(list(Server = "nginx/1.18.0 (Ubuntu)", 
        Date = "Mon, 06 Feb 2023 14:56:35 GMT", `Content-Type` = "application/json; charset=utf-8", 
        `Transfer-Encoding` = "chunked", Connection = "keep-alive", 
        Location = "/landmarks?id=eq.23014", `Content-Range` = "*/*", 
        `Content-Profile` = "public"), class = "httr2_headers"), 
    body = charToRaw("[{\"id\":23014,\"campaign_id\":15497,\"tree_code\":null,\"taxa_name\":\"Malus\",\"dbh\":850,\"dbh_unit\":null,\"azimut\":0,\"distance\":620,\"distance_unit\":\"mm\",\"geom\":{\"type\":\"Point\",\"coordinates\":[-79.42699,48.4367]},\"type\":\"gps\",\"notes\":null,\"created_at\":\"2023-02-06T09:56:35.532386-05:00\",\"updated_at\":\"2023-02-06T09:56:35.532386-05:00\",\"trap_id\":null,\"device_id\":null,\"lure_id\":10849,\"axis\":null,\"thermograph_type\":null}]")), class = "httr2_response")
