<?php

#[Attribute]
class Test {
	function __construct(array $data) {}
}

$data = [
    'value' => true
];

function method1(#[Test(['value' => true])] $data) {}

function method2(
	#[Test(['value' => true])]
	$data
) {}

function method3(
	#[Test([
        	'value' => true
	])]
	$data
) {}
