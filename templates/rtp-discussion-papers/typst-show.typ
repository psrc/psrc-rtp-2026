#show: rtp-discussion-report.with(
    $if(title)$
        title: "$title$",
    $endif$
    $if(params.discussion_topic)$
        discussion_topic: "$params.discussion_topic$",
    $endif$
    $if(params.discussion_date)$
        discussion_date: "$params.discussion_date$",
    $endif$
)