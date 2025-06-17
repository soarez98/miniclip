<?php

namespace Tests\Feature;

use Aws\DynamoDb\DynamoDbClient;
use Aws\Kms\KmsClient;
use Aws\Result;
use Mockery;
use Tests\TestCase;

/**
 * Summary of DataControllerTest
 */
class DataControllerTest extends TestCase
{
    /**
     * Summary of dynamoDbMock
     * @var 
     */
    protected $dynamoDbMock;
    /**
     * Summary of kmsMock
     * @var 
     */
    protected $kmsMock;

    /**
     * Summary of setUp
     * @return void
     */
    public function setUp(): void
    {
        parent::setUp();

        // Mock AWS clients using fully qualified class names
        $this->dynamoDbMock = $this->mock(abstract: DynamoDbClient::class);
        $this->kmsMock = $this->mock(abstract: KmsClient::class);

        // Bind mocks to Laravel's container
        $this->app->instance(abstract: DynamoDbClient::class, instance: $this->dynamoDbMock);
        $this->app->instance(abstract: KmsClient::class, instance: $this->kmsMock);
    }

    /**
     * Summary of tearDown
     * @return void
     */
    public function tearDown(): void
    {
        Mockery::close();
        parent::tearDown();
    }

    /**
     * Summary of testSetSuccess
     * @return void
     */
    public function testSetSuccess(): void
    {
        // Mock KMS encrypt
        $this->kmsMock->shouldReceive(methodNames: 'encrypt')
            ->once()
            ->with(args: [
                'KeyId' => config(key: 'aws.kms.key'),
                'Plaintext' => gzcompress(data: 'testvalue')
            ])
            ->andReturn(args: new Result(data: ['CiphertextBlob' => 'encrypted_data']));

        // Mock DynamoDB putItem
        $this->dynamoDbMock->shouldReceive(methodNames: 'putItem')
            ->once()
            ->with(args: [
                'TableName' => config(key: 'aws.dynamo.table'),
                'Item' => [
                    'key' => ['S' => 'testkey'],
                    'value' => ['B' => 'encrypted_data']
                ]
            ])
            ->andReturn(args: new Result());

        $response = $this->postJson(uri: '/set', data: [
            'key' => 'testkey',
            'value' => 'testvalue'
        ]);

        $response->assertStatus(status: 200)
                 ->assertJson(value: ['error' => 'ok']);
    }

    /**
     * Summary of testSetInvalidInput
     * @return void
     */
    public function testSetInvalidInput(): void
    {
        $response = $this->postJson(uri: '/set', data: [
            'key' => '', // Invalid: empty key
            'value' => 'testvalue'
        ]);

        $response->assertStatus(status: 422)
                 ->assertJsonValidationErrors(errors: ['key']);
    }

    /**
     * Summary of testGetSuccess
     * @return void
     */
    public function testGetSuccess(): void
    {
        // Mock DynamoDB getItem
        $this->dynamoDbMock->shouldReceive(methodNames: 'getItem')
            ->once()
            ->with(args: [
                'TableName' => config(key: 'aws.dynamo.table'),
                'Key' => [
                    'key' => ['S' => 'testkey']
                ]
            ])
            ->andReturn(args: new Result(data: ['Item' => [
                'key' => ['S' => 'testkey'],
                'value' => ['B' => 'encrypted_data']
            ]]));

        // Mock KMS decrypt
        $this->kmsMock->shouldReceive(methodNames: 'decrypt')
            ->once()
            ->with(args: ['CiphertextBlob' => 'encrypted_data'])
            ->andReturn(args: new Result(data: ['Plaintext' => gzcompress(data: 'testvalue')]));

        $response = $this->getJson(uri: '/get/testkey');

        $response->assertStatus(status: 200)
                 ->assertJson(value: [
                     'error' => 'ok',
                     'value' => 'testvalue'
                 ]);
    }

    /**
     * Summary of testGetNotFound
     * @return void
     */
    public function testGetNotFound(): void
    {
        // Mock DynamoDB getItem (no item)
        $this->dynamoDbMock->shouldReceive(methodNames: 'getItem')
            ->once()
            ->with(args: [
                'TableName' => config(key: 'aws.dynamo.table'),
                'Key' => [
                    'key' => ['S' => 'testkey']
                ]
            ])
            ->andReturn(args: new Result(data: []));

        $response = $this->getJson(uri: '/get/testkey');

        $response->assertStatus(status: 200)
                 ->assertJson(value: [
                     'error' => 'not_found',
                     'value' => null
                 ]);
    }
}