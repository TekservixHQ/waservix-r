# waservix: WAservix WhatsApp API Gateway for R

**WAservix WhatsApp API Gateway**: the official R client by Tekservix. Send WhatsApp texts, images, documents and locations from your own number.

**You need a Tekservix account with WAservix and a WhatsApp number connected in the dashboard.** Each number has its own API key (it starts with `wpk_`) under WAservix → Devices. [Create an account](https://tekservix.com/waservix) · [API docs](https://tekservix.com/waservix/docs/)

## Install

```r
install.packages("waservix")
# development version
# install.packages("remotes"); remotes::install_github("TekservixHQ/waservix-r")
```

## Use

```r
library(waservix)

client <- waservix_client("wpk_your_device_key")   # or set WASERVIX_API_KEY

waservix_send_text(client, "+250788123456", "Your order #1042 has shipped.")
waservix_send_image(client, "+250788123456", "https://example.com/catalogue.jpg", caption = "New arrivals")
waservix_send_document(client, "+250788123456", "https://example.com/inv-1042.pdf",
                       file_name = "Invoice-1042.pdf", caption = "Your invoice")
waservix_send_location(client, "+250788123456", -1.9441, 30.0619, name = "Our shop")

# Groups: IDs end in @g.us
groups <- waservix_list_groups(client)
waservix_get_status(client)
waservix_on_whatsapp(client, "+250788123456")
```

Every function returns the API's `data` as a list. API errors are raised as `waservix_error` conditions with `status`, `api_message` and `body`.

Webhooks: `waservix_verify_webhook(signature_header, secret)` checks `X-Webhook-Signature`.

## More WAservix integrations

[npm](https://www.npmjs.com/package/waservix) · [PyPI](https://pypi.org/project/waservix/) · [Java/Kotlin](https://central.sonatype.com/artifact/com.tekservix/waservix) · [All docs](https://tekservix.com/waservix/docs/)

support@tekservix.com · MIT licensed.
