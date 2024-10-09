#let rtp-discussion-report(
    title: "title",
    discussion_topic: "discussion_topic",
    discussion_date: "discussion_date",
    body,
) = {

    set text(
        font: "Poppins",
        size: 12pt,
    )

    set page(
    "us-letter",
   
    margin: (left: 0.75in, right: 0.75in, top: 1.5in, bottom: 0.75in),

    header: context{

        if counter(page).get().first() == 1 {
            align(left, image("images/rtp-logo-ferry.png", width: 100%))
        } else {
            align(left, image("images/rtp-pg2-logo.png", width: 100%))
        }
    },
    
    background: place(bottom, dx: 0.75in, dy: -0.60in, line(length: 7in, stroke: 6pt + rgb("BCBEC0"))),
    
    footer: context{

        let i = counter(page).get().first()

        if calc.odd(i) {
            align(
                grid(
                    columns: (60%, 40%),
                    align(left,  text(fill: rgb("4C4C4C"), size: 10pt, "RTP Emphasis Area: " + discussion_topic + " | " + discussion_date)),
                    align(right, text(fill: rgb("F05A28"), size: 10pt, weight: "bold", counter(page).display("1"))),
                )
            )
        } else {
            align(
                grid(
                    columns: (40%, 60%),
                    align(left, text(fill: rgb("F05A28"), size: 10pt, weight: "bold", counter(page).display("1"))),
                    align(right, text(fill: rgb("4C4C4C"), size: 10pt, "RTP Emphasis Area: " + discussion_topic + " | " + discussion_date)),
                )
            )
        }
    },

    )

    body
}
