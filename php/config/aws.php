<?php

return [
    "region" => env('AWS_DEFAULT_REGION', 'eu-central-1'),
    "dynamo" => [
        "table" => env('AWS_DYNAMO_TABLE', 'miniclip')
    ],
    "kms" => [
        "key"=> env('AWS_KMS_KEY')
    ]
];