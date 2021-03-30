package com.arlib.floatingsearchview;

/**
 * Copyright (C) 2015 Ari C.
 * <p/>
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * <p/>
 * http://www.apache.org/licenses/LICENSE-2.0
 * <p/>
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import android.app.Activity;
import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.os.Parcel;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;
import android.view.WindowManager;
import android.view.inputmethod.EditorInfo;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.appcompat.graphics.drawable.DrawerArrowDrawable;
import androidx.cardview.widget.CardView;
import androidx.core.graphics.drawable.DrawableCompat;
import androidx.core.view.ViewCompat;
import androidx.drawerlayout.widget.DrawerLayout;

import com.arlib.floatingsearchview.util.Util;
import com.arlib.floatingsearchview.util.adapter.TextWatcherAdapter;
import com.arlib.floatingsearchview.util.view.SearchInputView;
import com.lightning.walletapp.R;

/**
 * A search UI widget that implements a floating search box also called persistent
 * search.
 */
public class FloatingSearchView extends FrameLayout {
    //The CardView's top or bottom height used for its shadow
    private final static int CARD_VIEW_TOP_BOTTOM_SHADOW_HEIGHT = 3;
    //The CardView's (default) corner radius height
    private final static int CARD_VIEW_CORNERS_HEIGHT = 2;
    private final static int CARD_VIEW_CORNERS_AND_TOP_BOTTOM_SHADOW_HEIGHT =
            CARD_VIEW_TOP_BOTTOM_SHADOW_HEIGHT + CARD_VIEW_CORNERS_HEIGHT;

    private final static long CLEAR_BTN_FADE_ANIM_DURATION = 500;
    private final static int CLEAR_BTN_WIDTH_DP = 48;

    private final static boolean ATTRS_DISMISS_ON_OUTSIDE_TOUCH_DEFAULT = true;
    private final static boolean ATTRS_DISMISS_ON_KEYBOARD_DISMISS_DEFAULT = false;
    private final static boolean ATTRS_SEARCH_BAR_SHOW_SEARCH_KEY_DEFAULT = true;
    private final static int ATTRS_QUERY_TEXT_SIZE_SP_DEFAULT = 18;
    private final static int ATTRS_SEARCH_BAR_MARGIN_DEFAULT = 0;
    private final static boolean ATTRS_DISMISS_FOCUS_ON_ITEM_SELECTION_DEFAULT = false;

    private Activity mHostActivity;

    private View mMainLayout;
    private boolean mDismissOnOutsideTouch = true;
    private boolean mIsFocused;
    private OnFocusChangeListener mFocusChangeListener;
    private boolean mDismissFocusOnItemSelection = ATTRS_DISMISS_FOCUS_ON_ITEM_SELECTION_DEFAULT;

    private CardView mQuerySection;
    private OnSearchListener mSearchListener;
    private SearchInputView mSearchInput;
    private int mQueryTextSize;
    private boolean mCloseSearchOnSofteKeyboardDismiss;
    private String mTitleText;
    private boolean mIsTitleSet;
    private int mSearchInputTextColor = -1;
    private int mSearchInputHintColor = -1;
    private String mOldQuery = "";
    private OnQueryChangeListener mQueryListener;
    private DrawerArrowDrawable mMenuBtnDrawable;
    private int mLeftActionIconColor;
    private String mSearchHint;
    private boolean mShowSearchKey;
    private ImageView mClearButton;
    private int mClearBtnColor;
    private Drawable mIconClear;
    private int mBackgroundColor;
    private boolean mSkipQueryFocusChangeEvent;
    private boolean mSkipTextChangeEvent;
    private boolean mIsInitialLayout = true;
    private OnClearSearchActionListener mOnClearSearchActionListener;

    /**
     * Interface for implementing a listener to listen
     * to state changes in the query text.
     */
    public interface OnQueryChangeListener {

        /**
         * Called when the query has changed. It will
         * be invoked when one or more characters in the
         * query was changed.
         *
         * @param oldQuery the previous query
         * @param newQuery the new query
         */
        void onSearchTextChanged(String oldQuery, String newQuery);
    }

    /**
     * Interface for implementing a listener to listen
     * to when the current search has completed.
     */
    public interface OnSearchListener {
        /**
         * Called when the current search has completed
         * as a result of pressing search key in the keyboard.
         * <p/>
         * Note: This will only get called if
         * {@link FloatingSearchView#setShowSearchKey(boolean)}} is set to true.
         *
         * @param currentQuery the text that is currently set in the query TextView
         */
        void onSearchAction(String currentQuery);
    }

    /**
     * Interface for implementing a listener to listen
     * to for focus state changes.
     */
    public interface OnFocusChangeListener {

        /**
         * Called when the search bar has gained focus
         * and listeners are now active.
         */
        void onFocus();

        /**
         * Called when the search bar has lost focus
         * and listeners are no more active.
         */
        void onFocusCleared();
    }

    /**
     * Interface for implementing a callback to be
     * invoked when the clear search text action button
     * (the x to the right of the text) is clicked.
     */
    public interface OnClearSearchActionListener {

        /**
         * Called when the clear search text button
         * was clicked.
         */
        void onClearSearchClicked();
    }

    public FloatingSearchView(Context context) {
        this(context, null);
    }

    public FloatingSearchView(Context context, AttributeSet attrs) {
        super(context, attrs);
        init(attrs);
    }

    private void init(AttributeSet attrs) {

        mHostActivity = Util.getHostActivity(getContext());
        mMainLayout = inflate(getContext(), R.layout.floating_search_view_floating_search_layout, this);
        mQuerySection = (CardView) findViewById(R.id.search_query_section);
        mClearButton = (ImageView) findViewById(R.id.clear_btn);
        mSearchInput = (SearchInputView) findViewById(R.id.search_bar_text);
        initDrawables();
        mClearButton.setImageDrawable(mIconClear);
        setupViews(attrs);
    }

    private void initDrawables() {
        mMenuBtnDrawable = new DrawerArrowDrawable(getContext());
        mIconClear = Util.getWrappedDrawable(getContext(), R.drawable.ic_close_black_24dp);
    }

    @Override
    protected void onLayout(boolean changed, int l, int t, int r, int b) {
        super.onLayout(changed, l, t, r, b);

        if (mIsInitialLayout) {

            //we need to add 5dp to the mSuggestionsSection because we are
            //going to move it up by 5dp in order to cover the search bar's
            //shadow padding and rounded corners. We also need to add an additional 10dp to
            //mSuggestionsSection in order to hide mSuggestionListContainer's
            //rounded corners and shadow for both, top and bottom.
            int addedHeight = 3 * Util.dpToPx(CARD_VIEW_CORNERS_AND_TOP_BOTTOM_SHADOW_HEIGHT);
            mIsInitialLayout = false;
        }
    }

    private void setupViews(AttributeSet attrs) {

        if (attrs != null) {
            applyXmlAttributes(attrs);
        }

        setupQueryBar();
    }

    private void applyXmlAttributes(AttributeSet attrs) {

        TypedArray a = getContext().obtainStyledAttributes(attrs, R.styleable.FloatingSearchView);

        try {

            int searchBarWidth = a.getDimensionPixelSize(
                    R.styleable.FloatingSearchView_floatingSearch_searchBarWidth,
                    ViewGroup.LayoutParams.MATCH_PARENT);
            mQuerySection.getLayoutParams().width = searchBarWidth;
            int searchBarLeftMargin = a.getDimensionPixelSize(
                    R.styleable.FloatingSearchView_floatingSearch_searchBarMarginLeft,
                    ATTRS_SEARCH_BAR_MARGIN_DEFAULT);
            int searchBarTopMargin = a.getDimensionPixelSize(
                    R.styleable.FloatingSearchView_floatingSearch_searchBarMarginTop,
                    ATTRS_SEARCH_BAR_MARGIN_DEFAULT);
            int searchBarRightMargin = a.getDimensionPixelSize(
                    R.styleable.FloatingSearchView_floatingSearch_searchBarMarginRight,
                    ATTRS_SEARCH_BAR_MARGIN_DEFAULT);
            LayoutParams querySectionLP = (LayoutParams) mQuerySection.getLayoutParams();

            int cardPadding = Util.dpToPx(CARD_VIEW_TOP_BOTTOM_SHADOW_HEIGHT);
            querySectionLP.setMargins(searchBarLeftMargin, searchBarTopMargin,
                    searchBarRightMargin, 0);
            mQuerySection.setLayoutParams(querySectionLP);

            setQueryTextSize(a.getDimensionPixelSize(R.styleable.FloatingSearchView_floatingSearch_searchInputTextSize,
                    ATTRS_QUERY_TEXT_SIZE_SP_DEFAULT));
            setSearchHint(a.getString(R.styleable.FloatingSearchView_floatingSearch_searchHint));
            setShowSearchKey(a.getBoolean(R.styleable.FloatingSearchView_floatingSearch_showSearchKey,
                    ATTRS_SEARCH_BAR_SHOW_SEARCH_KEY_DEFAULT));
            setCloseSearchOnKeyboardDismiss(a.getBoolean(R.styleable.FloatingSearchView_floatingSearch_close_search_on_keyboard_dismiss,
                    ATTRS_DISMISS_ON_KEYBOARD_DISMISS_DEFAULT));
            setDismissOnOutsideClick(a.getBoolean(R.styleable.FloatingSearchView_floatingSearch_dismissOnOutsideTouch,
                    ATTRS_DISMISS_ON_OUTSIDE_TOUCH_DEFAULT));
            setDismissFocusOnItemSelection(a.getBoolean(R.styleable.FloatingSearchView_floatingSearch_dismissFocusOnItemSelection,
                    ATTRS_DISMISS_FOCUS_ON_ITEM_SELECTION_DEFAULT));
            setBackgroundColor(a.getColor(R.styleable.FloatingSearchView_floatingSearch_backgroundColor
                    , Util.getColor(getContext(), R.color.background)));
            setLeftActionIconColor(a.getColor(R.styleable.FloatingSearchView_floatingSearch_leftActionColor
                    , Util.getColor(getContext(), R.color.left_action_icon)));
            setClearBtnColor(a.getColor(R.styleable.FloatingSearchView_floatingSearch_clearBtnColor
                    , Util.getColor(getContext(), R.color.clear_btn_color)));
            int viewTextColor = a.getColor(R.styleable.FloatingSearchView_floatingSearch_viewTextColor
                    , Util.getColor(getContext(), R.color.dark_gray));
            setViewTextColor(viewTextColor);
            setQueryTextColor(a.getColor(R.styleable.FloatingSearchView_floatingSearch_viewSearchInputTextColor
                    , viewTextColor));
            setHintTextColor(a.getColor(R.styleable.FloatingSearchView_floatingSearch_hintTextColor
                    , Util.getColor(getContext(), R.color.hint_color)));
        } finally {
            a.recycle();
        }
    }

    private void setupQueryBar() {

        mSearchInput.setTextColor(mSearchInputTextColor);
        mSearchInput.setHintTextColor(mSearchInputHintColor);

        if (!isInEditMode() && mHostActivity != null) {
            mHostActivity.getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_ADJUST_PAN);
        }

        ViewTreeObserver vto = mQuerySection.getViewTreeObserver();
        vto.addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
            @Override
            public void onGlobalLayout() {
                Util.removeGlobalLayoutObserver(mQuerySection, this);
            }
        });

        mClearButton.setVisibility(View.INVISIBLE);
        mClearButton.setOnClickListener(new OnClickListener() {
            @Override
            public void onClick(View v) {
                mSearchInput.setText("");
                if (mOnClearSearchActionListener != null) {
                    mOnClearSearchActionListener.onClearSearchClicked();
                }
            }
        });

        mSearchInput.addTextChangedListener(new TextWatcherAdapter() {

            public void onTextChanged(final CharSequence s, int start, int before, int count) {
                //todo investigate why this is called twice when pressing back on the keyboard

                if (mSkipTextChangeEvent) {
                    mSkipTextChangeEvent = false;
                } else {
                    if (mSearchInput.getText().toString().length() != 0 &&
                            mClearButton.getVisibility() == View.INVISIBLE) {
                        mClearButton.setAlpha(0.0f);
                        mClearButton.setVisibility(View.VISIBLE);
                        ViewCompat.animate(mClearButton).alpha(1.0f).setDuration(CLEAR_BTN_FADE_ANIM_DURATION).start();
                    } else if (mSearchInput.getText().toString().length() == 0) {
                        mClearButton.setVisibility(View.INVISIBLE);
                    }

                    if (mQueryListener != null && !mOldQuery.equals(mSearchInput.getText().toString())) {
                        mQueryListener.onSearchTextChanged(mOldQuery, mSearchInput.getText().toString());
                    }

                }

                mOldQuery = mSearchInput.getText().toString();
            }

        });

        mSearchInput.setOnFocusChangeListener(new TextView.OnFocusChangeListener() {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {

                if (mSkipQueryFocusChangeEvent) {
                    mSkipQueryFocusChangeEvent = false;
                } else if (hasFocus != mIsFocused) {
                    setSearchFocusedInternal(hasFocus);
                }
            }
        });

        mSearchInput.setOnKeyboardDismissedListener(new SearchInputView.OnKeyboardDismissedListener() {
            @Override
            public void onKeyboardDismissed() {
                if (mCloseSearchOnSofteKeyboardDismiss) {
                    setSearchFocusedInternal(false);
                }
            }
        });

        mSearchInput.setOnSearchKeyListener(new SearchInputView.OnKeyboardSearchKeyClickListener() {
            @Override
            public void onSearchKeyClicked() {
                if (mSearchListener != null) {
                    mSearchListener.onSearchAction(getQuery());
                }
                mSkipTextChangeEvent = true;
                mSkipTextChangeEvent = true;
                if (mIsTitleSet) {
                    setSearchBarTitle(getQuery());
                } else {
                    setSearchText(getQuery());
                }
                setSearchFocusedInternal(false);
            }
        });
    }

    //ensures that the end margin of the search input is according to Material specs
    private void handleOnVisibleMenuItemsWidthChanged(int menuItemsWidth) {
        if (menuItemsWidth == 0) {
            mClearButton.setTranslationX(-Util.dpToPx(4));
            int paddingRight = Util.dpToPx(4);
            if (mIsFocused) {
                paddingRight += Util.dpToPx(CLEAR_BTN_WIDTH_DP);
            } else {
                paddingRight += Util.dpToPx(14);
            }
            mSearchInput.setPadding(0, 0, paddingRight, 0);
        } else {
            mClearButton.setTranslationX(-menuItemsWidth);
            int paddingRight = menuItemsWidth;
            if (mIsFocused) {
                paddingRight += Util.dpToPx(CLEAR_BTN_WIDTH_DP);
            }
            mSearchInput.setPadding(0, 0, paddingRight, 0);
        }
    }

    /**
     * Sets the menu button's color.
     *
     * @param color the color to be applied to the
     *              left menu button.
     */
    public void setLeftActionIconColor(int color) {
        mLeftActionIconColor = color;
        mMenuBtnDrawable.setColor(color);
    }

    /**
     * Sets the clear button's color.
     *
     * @param color the color to be applied to the
     *              clear button.
     */
    public void setClearBtnColor(int color) {
        mClearBtnColor = color;
        DrawableCompat.setTint(mIconClear, mClearBtnColor);
    }

    /**
     * Sets the background color of the search
     * view including the suggestions section.
     *
     * @param color the color to be applied to the search bar and
     *              the suggestion section background.
     */
    public void setBackgroundColor(int color) {
        mBackgroundColor = color;
    }

    /**
     * Sets the text color of the search
     * and suggestion text.
     *
     * @param color the color to be applied to the search and suggestion
     *              text.
     */
    public void setViewTextColor(int color) {
        setQueryTextColor(color);
    }

    /**
     * Sets whether the search will lose focus when a suggestion item is clicked.
     *
     * @param dismissFocusOnItemSelection
     */
    public void setDismissFocusOnItemSelection(boolean dismissFocusOnItemSelection) {
        mDismissFocusOnItemSelection = dismissFocusOnItemSelection;
    }

    /**
     * Sets the text color of the search text.
     *
     * @param color
     */
    public void setQueryTextColor(int color) {
        mSearchInputTextColor = color;
        if (mSearchInput != null) {
            mSearchInput.setTextColor(mSearchInputTextColor);
        }
    }

    /**
     * Set the text size of the text in the search box.
     *
     * @param sizePx
     */
    public void setQueryTextSize(int sizePx) {
        mQueryTextSize = sizePx;
        mSearchInput.setTextSize(mQueryTextSize);
    }

    /**
     * Sets the text color of the search
     * hint.
     *
     * @param color the color to be applied to the search hint.
     */
    public void setHintTextColor(int color) {
        mSearchInputHintColor = color;
        if (mSearchInput != null) {
            mSearchInput.setHintTextColor(color);
        }
    }

    /**
     * Set a hint that will appear in the
     * search input. Default hint is R.string.abc_search_hint
     * which is "search..." (when device language is set to english)
     *
     * @param searchHint
     */
    public void setSearchHint(String searchHint) {
        mSearchHint = searchHint != null ? searchHint : getResources().getString(R.string.abc_search_hint);
        mSearchInput.setHint(mSearchHint);
    }

    /**
     * Sets whether the the button with the search icon
     * will appear in the soft-keyboard or not.
     *
     * @param show to show the search button in
     *             the soft-keyboard.
     */
    public void setShowSearchKey(boolean show) {
        mShowSearchKey = show;
        if (show) {
            mSearchInput.setImeOptions(EditorInfo.IME_ACTION_SEARCH);
        } else {
            mSearchInput.setImeOptions(EditorInfo.IME_ACTION_NONE);
        }
    }


    /**
     * Sets whether the search will lose focus when the softkeyboard
     * gets closed from a back press
     *
     * @param closeSearchOnKeyboardDismiss
     */
    public void setCloseSearchOnKeyboardDismiss(boolean closeSearchOnKeyboardDismiss) {
        this.mCloseSearchOnSofteKeyboardDismiss = closeSearchOnKeyboardDismiss;
    }

    /**
     * Set whether a touch outside of the
     * search bar's bounds will cause the search bar to
     * loos focus.
     *
     * @param enable true to dismiss on outside touch, false otherwise.
     */
    public void setDismissOnOutsideClick(boolean enable) {
        mDismissOnOutsideTouch = enable;
    }

    /**
     * Wrapper implementation for EditText.setFocusable(boolean focusable)
     *
     * @param focusable true, to make search focus when
     *                  clicked.
     */
    public void setSearchFocusable(boolean focusable) {
        mSearchInput.setFocusable(focusable);
        mSearchInput.setFocusableInTouchMode(focusable);
    }

    /**
     * Sets the title for the search bar.
     * <p/>
     * Note that after the title is set, when
     * the search gains focus, the title will be replaced
     * by the search hint.
     *
     * @param title the title to be shown when search
     *              is not focused
     */
    public void setSearchBarTitle(CharSequence title) {
        this.mTitleText = title.toString();
        mIsTitleSet = true;
        mSearchInput.setText(title);
    }

    /**
     * Sets the search text.
     * <p/>
     * Note that this is the different from
     * {@link #setSearchBarTitle(CharSequence title) setSearchBarTitle} in
     * that it keeps the text when the search gains focus.
     *
     * @param text the text to be set for the search
     *             input.
     */
    public void setSearchText(CharSequence text) {
        mIsTitleSet = false;
        setQueryText(text);
    }

    /**
     * Returns the current query text.
     *
     * @return the current query
     */
    public String getQuery() {
        return mOldQuery;
    }

    public void clearQuery() {
        mSearchInput.setText("");
    }

    /**
     * Sets whether the search is focused or not.
     *
     * @param focused true, to set the search to be active/focused.
     * @return true if the search was focused and will now become not focused. Useful for
     * calling supper.onBackPress() in the hosting activity only if this method returns false
     */
    public boolean setSearchFocused(final boolean focused) {

        boolean updatedToNotFocused = !focused && this.mIsFocused;

        return updatedToNotFocused;
    }

    private void setQueryText(CharSequence text) {
        mSearchInput.setText(text);
        //move cursor to end of text
        mSearchInput.setSelection(mSearchInput.getText().length());
    }

    public void clearSearchFocus() {
        setSearchFocusedInternal(false);
    }

    public boolean isSearchBarFocused() {
        return mIsFocused;
    }

    private void setSearchFocusedInternal(final boolean focused) {
        this.mIsFocused = focused;

        if (focused) {
            mSearchInput.requestFocus();
            handleOnVisibleMenuItemsWidthChanged(0);//this must be called before  mMenuView.hideIfRoomItems(...)
            Util.showSoftKeyboard(getContext(), mSearchInput);
            if (mIsTitleSet) {
                mSkipTextChangeEvent = true;
                mSearchInput.setText("");
            } else {
                mSearchInput.setSelection(mSearchInput.getText().length());
            }
            mSearchInput.setLongClickable(true);
            mClearButton.setVisibility((mSearchInput.getText().toString().length() == 0) ?
                    View.INVISIBLE : View.VISIBLE);
            if (mFocusChangeListener != null) {
                mFocusChangeListener.onFocus();
            }
        } else {
            mMainLayout.requestFocus();
            handleOnVisibleMenuItemsWidthChanged(0);//this must be called before  mMenuView.hideIfRoomItems(...)
            mClearButton.setVisibility(View.GONE);
            if (mHostActivity != null) {
                Util.closeSoftKeyboard(mHostActivity);
            }
            if (mIsTitleSet) {
                mSkipTextChangeEvent = true;
                mSearchInput.setText(mTitleText);
            }
            mSearchInput.setLongClickable(false);
            if (mFocusChangeListener != null) {
                mFocusChangeListener.onFocusCleared();
            }
        }
    }

    /**
     * Sets the listener that will listen for query
     * changes as they are being typed.
     *
     * @param listener listener for query changes
     */
    public void setOnQueryChangeListener(OnQueryChangeListener listener) {
        this.mQueryListener = listener;
    }

    @Override
    public Parcelable onSaveInstanceState() {
        Parcelable superState = super.onSaveInstanceState();
        SavedState savedState = new SavedState(superState);
        savedState.isFocused = mIsFocused;
        savedState.query = getQuery();
        savedState.searchHint = mSearchHint;
        savedState.dismissOnOutsideClick = mDismissOnOutsideTouch;
        savedState.showSearchKey = mShowSearchKey;
        savedState.isTitleSet = mIsTitleSet;
        savedState.backgroundColor = mBackgroundColor;
        savedState.queryTextColor = mSearchInputTextColor;
        savedState.searchHintTextColor = mSearchInputHintColor;
        savedState.leftIconColor = mLeftActionIconColor;
        savedState.clearBtnColor = mClearBtnColor;
        savedState.queryTextSize = mQueryTextSize;
        savedState.dismissOnSoftKeyboardDismiss = mDismissOnOutsideTouch;
        savedState.dismissFocusOnSuggestionItemClick = mDismissFocusOnItemSelection;
        return savedState;
    }

    @Override
    public void onRestoreInstanceState(Parcelable state) {
        final SavedState savedState = (SavedState) state;
        super.onRestoreInstanceState(savedState.getSuperState());
        mIsFocused = savedState.isFocused;
        mIsTitleSet = savedState.isTitleSet;
        mOldQuery = savedState.query;
        setSearchText(mOldQuery);
        setDismissOnOutsideClick(savedState.dismissOnOutsideClick);
        setShowSearchKey(savedState.showSearchKey);
        setSearchHint(savedState.searchHint);
        setBackgroundColor(savedState.backgroundColor);
        setQueryTextColor(savedState.queryTextColor);
        setQueryTextSize(savedState.queryTextSize);
        setHintTextColor(savedState.searchHintTextColor);
        setLeftActionIconColor(savedState.leftIconColor);
        setClearBtnColor(savedState.clearBtnColor);
        setCloseSearchOnKeyboardDismiss(savedState.dismissOnSoftKeyboardDismiss);
        setDismissFocusOnItemSelection(savedState.dismissFocusOnSuggestionItemClick);

        if (mIsFocused) {
            mSkipTextChangeEvent = true;
            mSkipQueryFocusChangeEvent = true;
            mClearButton.setVisibility((savedState.query.length() == 0) ? View.INVISIBLE : View.VISIBLE);
            Util.showSoftKeyboard(getContext(), mSearchInput);
        }
    }

    static class SavedState extends BaseSavedState {
        private boolean isFocused;
        private String query;
        private int queryTextSize;
        private int suggestionTextSize;
        private String searchHint;
        private boolean dismissOnOutsideClick;
        private boolean showMoveSuggestionUpBtn;
        private boolean showSearchKey;
        private boolean isTitleSet;
        private int backgroundColor;
        private int suggestionsTextColor;
        private int queryTextColor;
        private int searchHintTextColor;
        private int actionOverflowMenuColor;
        private int menuItemIconColor;
        private int leftIconColor;
        private int clearBtnColor;
        private int suggestionUpBtnColor;
        private int dividerColor;
        private int menuId;
        private int leftActionMode;
        private boolean dimBackground;
        private long suggestionsSectionAnimSuration;
        private boolean dismissOnSoftKeyboardDismiss;
        private boolean dismissFocusOnSuggestionItemClick;

        SavedState(Parcelable superState) {
            super(superState);
        }

        private SavedState(Parcel in) {
            super(in);
            isFocused = (in.readInt() != 0);
            query = in.readString();
            queryTextSize = in.readInt();
            suggestionTextSize = in.readInt();
            searchHint = in.readString();
            dismissOnOutsideClick = (in.readInt() != 0);
            showMoveSuggestionUpBtn = (in.readInt() != 0);
            showSearchKey = (in.readInt() != 0);
            isTitleSet = (in.readInt() != 0);
            backgroundColor = in.readInt();
            suggestionsTextColor = in.readInt();
            queryTextColor = in.readInt();
            searchHintTextColor = in.readInt();
            actionOverflowMenuColor = in.readInt();
            menuItemIconColor = in.readInt();
            leftIconColor = in.readInt();
            clearBtnColor = in.readInt();
            suggestionUpBtnColor = in.readInt();
            dividerColor = in.readInt();
            menuId = in.readInt();
            leftActionMode = in.readInt();
            dimBackground = (in.readInt() != 0);
            suggestionsSectionAnimSuration = in.readLong();
            dismissOnSoftKeyboardDismiss = (in.readInt() != 0);
            dismissFocusOnSuggestionItemClick = (in.readInt() != 0);
        }

        @Override
        public void writeToParcel(Parcel out, int flags) {
            super.writeToParcel(out, flags);
            out.writeInt(isFocused ? 1 : 0);
            out.writeString(query);
            out.writeInt(queryTextSize);
            out.writeInt(suggestionTextSize);
            out.writeString(searchHint);
            out.writeInt(dismissOnOutsideClick ? 1 : 0);
            out.writeInt(showMoveSuggestionUpBtn ? 1 : 0);
            out.writeInt(showSearchKey ? 1 : 0);
            out.writeInt(isTitleSet ? 1 : 0);
            out.writeInt(backgroundColor);
            out.writeInt(suggestionsTextColor);
            out.writeInt(queryTextColor);
            out.writeInt(searchHintTextColor);
            out.writeInt(actionOverflowMenuColor);
            out.writeInt(menuItemIconColor);
            out.writeInt(leftIconColor);
            out.writeInt(clearBtnColor);
            out.writeInt(suggestionUpBtnColor);
            out.writeInt(dividerColor);
            out.writeInt(menuId);
            out.writeInt(leftActionMode);
            out.writeInt(dimBackground ? 1 : 0);
            out.writeLong(suggestionsSectionAnimSuration);
            out.writeInt(dismissOnSoftKeyboardDismiss ? 1 : 0);
            out.writeInt(dismissFocusOnSuggestionItemClick ? 1 : 0);
        }

        public static final Creator<SavedState> CREATOR
                = new Creator<SavedState>() {
            public SavedState createFromParcel(Parcel in) {
                return new SavedState(in);
            }

            public SavedState[] newArray(int size) {
                return new SavedState[size];
            }
        };
    }

    private DrawerLayout.DrawerListener mDrawerListener = new DrawerListener();

    @Override
    protected void onDetachedFromWindow() {
        super.onDetachedFromWindow();
    }

    private class DrawerListener implements DrawerLayout.DrawerListener {
        @Override
        public void onDrawerSlide(View drawerView, float slideOffset) {

        }

        @Override
        public void onDrawerOpened(View drawerView) {

        }

        @Override
        public void onDrawerClosed(View drawerView) {

        }

        @Override
        public void onDrawerStateChanged(int newState) {

        }
    }
}
