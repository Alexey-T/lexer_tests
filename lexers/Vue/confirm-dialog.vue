<template>
    <basic-dialog>
        <slot></slot>
        <footer>
            <button @click="onCancel">{{cancel}}</button>
            <button v-focus="_focus"
                    class="primary"
                    @click="onOK">{{ok}}</button>
        </footer>
    </basic-dialog>
</template>
<script type="text/babel">
    import {focus} from 'vue-focus'
    import BasicDialog from '../basic-dialog';

    export default {
        directives: {focus},
        components: {BasicDialog},
        props: {
            ok: {
                type: String,
                default: 'OK'
            },
            cancel: {
                type: String,
                default: 'Cancel'
            },
            _focus: {
                type:String,
                default: false
            }
        },
        methods: {
            show () {
                this.$children[0].show();
                this._focus = true;
                return new Promise((resolve, reject) => {
                    this._resolve = resolve;
                    this._reject = reject;
                });
            },
            hide () {
                this.$children[0].hide();
                this._focus = false;
            },
            onOK () {
                this.hide();
                this._resolve();
                this.$dispatch('ok');
            },
            onCancel () {
                this.hide();
                this._reject();
                this.$dispatch('cancel');
            }
        }
    };
</script>
