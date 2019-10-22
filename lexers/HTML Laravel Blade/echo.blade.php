Hello, {{{ $name }}}.
timestamp is {{{ time() }}}.

{{{ isset($name) ? $name : 'Default' }}}
@{{ This will not be processed by Blade }}

@include('view.name', array('some'=>'data'))

@extends('list.item.container')

@section('list.item.content')
    <p>This is an item of type {{ $item->type }}</p>
@overwrite
