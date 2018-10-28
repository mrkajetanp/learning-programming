package com.cajetan.maintestingapp.media;

import android.content.Context;
import android.os.Build;
import android.util.AttributeSet;
import android.view.View;
import android.webkit.WebView;

public class MediaWebView extends WebView {

    public MediaWebView(Context context) {
        super(context);
    }

    public MediaWebView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public MediaWebView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected void onWindowVisibilityChanged(int visibility) {
        if (visibility == View.INVISIBLE)
            super.onWindowVisibilityChanged(View.VISIBLE);
    }
}
