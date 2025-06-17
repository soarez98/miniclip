<?php

namespace App\Services;

use App\Exceptions\DynamoKeyNotFoundException;
use Aws\DynamoDb\DynamoDbClient;
use Aws\Kms\KmsClient;

/**
 * Summary of DataService
 */
final class DataService
{
    /**
     * Summary of __construct
     * @param \Aws\DynamoDb\DynamoDbClient $dynamoclient
     * @param \Aws\Kms\KmsClient $kmsclient
     */
    public function __construct(
        private DynamoDbClient $dynamoclient = new DynamoDbClient(['region' => config('aws.region'), 'version' => 'latest']),
        private KmsClient $kmsclient = new KmsClient(['region' => config('aws.region'), 'version' => 'latest']),
    ){}

    /**
     * Summary of get
     * @param string $key
     * @throws \App\Exceptions\DynamoKeyNotFoundException
     * @return mixed
     */
    public function get(string $key): mixed
    {
        // Get the key -> value
        $item = $this->dynamoclient->getItem(args: [
            'TableName' => config(key: 'aws.dynamo.table'),
            'Key' => $key,
        ]);

        // Key does not exists
        if (!isset($item['Item'])) {
            throw new DynamoKeyNotFoundException(message: 'not_found');
        }

        // Decrypt the value
        return $this->kmsclient->decrypt(args: ['CiphertextBlob' => $item['Item']['value']['B']]);
    }

    /**
     * Summary of set
     * @param array $attributes
     * @return \Aws\Result
     */
    public function set(array $attributes): mixed
    {
        //Encrypt the value
        $encrypted = $this->kmsclient->encrypt(args: [
            'KeyId' => config(key: 'aws.kms.key'),
            'Plaintext' => $attributes['value']
        ]);

        // Insert the encrypted value
        return $this->dynamoclient->putItem(args: [
            'TableName' => config(key: 'aws.dynamo.table'),
            'Item' => [
                'key' => ['S' => $attributes['key']],
                'value' => ['B' => $encrypted['CiphertextBlob']]
            ],
        ]);
    }
}