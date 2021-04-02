package me.relex.circleindicator;

import android.view.Gravity;
import android.widget.LinearLayout;
import com.lightning.walletapp.R;

public class Config {
    int backgroundResId = R.drawable.indicator_item;
    int orientation = LinearLayout.HORIZONTAL;
    int gravity = Gravity.START;

    Config() {
    }

    public static class Builder {

        private final Config mConfig;

        public Builder() {
            mConfig = new Config();
        }

        public Builder drawable(int backgroundResId) {
            mConfig.backgroundResId = backgroundResId;
            return this;
        }

        public Builder gravity(int gravity) {
            mConfig.gravity = gravity;
            return this;
        }

        public Config build() {
            return mConfig;
        }
    }
}
