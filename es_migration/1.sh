curl -XPOST 'localhost:9200/slacknowledge/_update_by_query?pretty' -H 'Content-Type: application/json' -d'
{
  "script": {
    "inline": "ctx._source.channel_id = '\''C5666B6BB'\''",
    "lang": "painless"
  }
}
'
