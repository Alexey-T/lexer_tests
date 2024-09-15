
/*
 * HTML Templates
 */

var html = html` <p>${content}</p> `

var html = html`

    <script type="text/javascript">
        var ${name} = "Value ${interpol}"
    </script>

    <script type="text/json">
        {

            "key1": "val${ue}",

            ${key}: ${value},

            "key2": [${val1}, "val${no}"],
        }
    </script>

    <style type="text/css">
        tr, .${sel} {

            background-${attr}: ${value};
        }
    </style>

    <p style="width: ${width}%" class="${class_name}" onclick="${click}">${content}</p>
    `

/*
 * JSON Templates
 */

var json = json` { "key": "value" } `

var json = json`
    {

        "key1": "val${ue}",

        ${key}: ${value},

        "key2": [${val1}, "val${no}"],
    }
    `

/*
 * JavaScript Templates
 */

var script = js` var = 0 `

var script = js`

    var ${name} = "Value ${interpol}"
    `

/*
 * CSS Templates
 */


var style = css` tr {  } `

var style = css`

    tr, .${sel} {

        background-${attr}: ${value};
    }
    `

/*
 * Unknown Template
 */

var other = other`
    Any content ${type}.
    `

var other = `
    Any content ${type}.
    `
