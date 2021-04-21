package com.indicator;

import immortan.Channel$;
import android.content.Context;
import android.util.DisplayMetrics;
import android.widget.LinearLayout;
import android.util.AttributeSet;
import com.lightning.walletapp.R;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;


public class ChannelIndicatorLine extends LinearLayout {
    private final static int DEFAULT_INDICATOR_HEIGHT = 8;

    protected int mIndicatorMargin = -1;
    protected int mIndicatorHeight = -1;
    protected int mIndicatorWidth = -1;

    public ChannelIndicatorLine(Context context) {
        super(context);
        initialize();
    }

    public ChannelIndicatorLine(Context context, AttributeSet attrs) {
        super(context, attrs);
        initialize();
    }

    public ChannelIndicatorLine(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initialize();
    }

    public ChannelIndicatorLine(Context context, AttributeSet attrs, int defStyleAttr, int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        initialize();
    }

    public void initialize() {
        DisplayMetrics metrics = getResources().getDisplayMetrics();
        int miniSize = (int) (TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, DEFAULT_INDICATOR_HEIGHT, metrics) + 0.5f);

        mIndicatorHeight = miniSize;
        mIndicatorWidth = miniSize * 3;
        mIndicatorMargin = 5;

        setOrientation(HORIZONTAL);
        setGravity(Gravity.START);
    }

    public void createIndicators(String[] states) {
        // Diff View
        int childViewCount = getChildCount();
        int count = states.length;

        if (count < childViewCount) {
            removeViews(count, childViewCount - count);
        } else if (count > childViewCount) {
            int addCount = count - childViewCount;
            int orientation = getOrientation();
            for (int i = 0; i < addCount; i++) {
                addIndicator(orientation);
            }
        }

        // Bind Style
        View indicator;
        for (int i = 0; i < count; i++) {
            indicator = getChildAt(i);
            String state = states[i];

            if (Channel$.MODULE$.SLEEPING().equals(state) || Channel$.MODULE$.WAIT_FUNDING_DONE().equals(state)) {
                indicator.setBackgroundResource(R.drawable.indicator_item_open_sleeping_wait);
                indicator.setAlpha(0.25f);
            } else if (Channel$.MODULE$.CLOSING().equals(state)) {
                indicator.setBackgroundResource(R.drawable.indicator_item_closing_suspended);
                indicator.setAlpha(0.6f);
            } else if (Channel$.MODULE$.SUSPENDED().equals(state)) {
                indicator.setBackgroundResource(R.drawable.indicator_item_closing_suspended);
                indicator.setAlpha(0.6f);
            } else {
                indicator.setBackgroundResource(R.drawable.indicator_item_open_sleeping_wait);
                indicator.setAlpha(1f);
            }
        }
    }

    protected void addIndicator(int orientation) {
        View indicator = new View(getContext());
        final LayoutParams params = generateDefaultLayoutParams();
        params.width = mIndicatorWidth;
        params.height = mIndicatorHeight;
        if (orientation == HORIZONTAL) {
            params.leftMargin = mIndicatorMargin;
            params.rightMargin = mIndicatorMargin;
        } else {
            params.topMargin = mIndicatorMargin;
            params.bottomMargin = mIndicatorMargin;
        }
        addView(indicator, params);
    }
}
