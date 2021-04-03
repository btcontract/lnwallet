package me.relex.circleindicator;

import android.content.Context;
import android.content.res.TypedArray;
import android.widget.LinearLayout;
import com.lightning.walletapp.R;
import android.util.AttributeSet;
import android.util.TypedValue;
import android.view.Gravity;
import android.view.View;

class BaseIndicatorLine extends LinearLayout {

    private final static int DEFAULT_INDICATOR_HEIGHT = 8;

    protected int mIndicatorMargin = -1;
    protected int mIndicatorWidth = -1;
    protected int mIndicatorHeight = -1;

    protected int mIndicatorBackgroundResId;

    public BaseIndicatorLine(Context context) {
        super(context);
        init(context, null);
    }

    public BaseIndicatorLine(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(context, attrs);
    }

    public BaseIndicatorLine(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        init(context, attrs);
    }

    public BaseIndicatorLine(Context context, AttributeSet attrs, int defStyleAttr,
                             int defStyleRes) {
        super(context, attrs, defStyleAttr, defStyleRes);
        init(context, attrs);
    }

    private void init(Context context, AttributeSet attrs) {
        Config config = handleTypedArray(context, attrs);
        initialize(config);
    }

    private Config handleTypedArray(Context context, AttributeSet attrs) {
        Config config = new Config();

        if (attrs == null) {
            return config;
        }

        TypedArray typedArray = context.obtainStyledAttributes(attrs, R.styleable.BaseIndicatorLine);
        config.backgroundResId = typedArray.getResourceId(R.styleable.BaseIndicatorLine_ci_drawable, R.drawable.indicator_item_based);
        typedArray.recycle();

        return config;
    }

    public void initialize(Config config) {
        int miniSize = (int) (TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, DEFAULT_INDICATOR_HEIGHT, getResources().getDisplayMetrics()) + 0.5f);

        mIndicatorHeight = miniSize;
        mIndicatorWidth = miniSize * 3;
        mIndicatorMargin = 3;

        mIndicatorBackgroundResId = (config.backgroundResId == 0) ? R.drawable.indicator_item_based : config.backgroundResId;

        setOrientation(config.orientation == VERTICAL ? VERTICAL : HORIZONTAL);
        setGravity(config.gravity >= 0 ? config.gravity : Gravity.CENTER);
    }

    public void createIndicators(int count) {
        // Diff View
        int childViewCount = getChildCount();
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
            indicator.setBackgroundResource(mIndicatorBackgroundResId);
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
